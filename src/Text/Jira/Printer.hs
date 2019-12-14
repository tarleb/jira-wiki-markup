{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Text.Jira.Parser
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Generate Jira wiki markup text from an abstract syntax tree.
-}
module Text.Jira.Printer
  ( pretty
  , renderBlock
  , renderInline
  , prettyBlocks
  , prettyInlines
  , JiraPrinter
  , PrinterState (..)
  , startState
  , withDefault
  ) where

import Data.Char (isAlphaNum)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Control.Monad ((<=<))
import Control.Monad.Reader (Reader, runReader, asks, local)
import Data.Text (Text)
import Text.Jira.Markup
import qualified Data.Text as T

-- | Render Jira document as Jira wiki formatted text.
pretty :: Doc -> Text
pretty (Doc blks) = prettyBlocks blks

-- | Render a list of Jira blocks as Jira wiki formatted text.
prettyBlocks :: [Block] -> Text
prettyBlocks blks = runReader (renderBlocks blks) startState

-- | Renders a list of Jira inline markup elements.
prettyInlines :: [Inline] -> Text
prettyInlines = \case
  [] ->
    ""
  s@Str{} : Styled style inlns : rest ->
    renderInline s <> renderStyledSafely style inlns <> prettyInlines rest
  Styled style inlns : s@(Str t) : rest | startsWithAlphaNum t ->
    renderStyledSafely style inlns <> renderInline s <> prettyInlines rest
  s@Str{} : SpecialChar c : rest@(Str {}:_) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  s@Space : SpecialChar c : rest@(Space {}:_) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  s@Linebreak : SpecialChar c : rest@(Space {}:_) ->
    (renderInline s `T.snoc` c) <> prettyInlines rest
  (x:xs) ->
    renderInline x <> prettyInlines xs

  where
    startsWithAlphaNum t = case T.uncons t of
      Just (c, _) -> isAlphaNum c
      _           -> False

-- | Internal state used by the printer.
data PrinterState = PrinterState
  { stateInTable   :: Bool
  , stateListLevel :: Text
  }

type JiraPrinter a = Reader PrinterState a

-- | Run with default state.
withDefault :: JiraPrinter a -> a
withDefault = flip runReader startState

-- | Default start state of the printer.
startState :: PrinterState
startState = PrinterState
  { stateInTable = False
  , stateListLevel = ""
  }

-- | Render a block as Jira wiki format.
renderBlocks :: [Block] -> JiraPrinter Text
renderBlocks = concatBlocks <=< mapM renderBlock

-- | Combine the texts produced from rendering a list of blocks.
concatBlocks :: [Text] -> JiraPrinter Text
concatBlocks blks = do
  listLevel <- asks stateListLevel
  inTable   <- asks stateInTable
  return $
    -- add final newline only if we are neither within a table nor a list.
    if inTable || not (T.null listLevel)
    then T.intercalate "\n" blks
    else T.unlines blks

-- | Render a block as Jira wiki format.
renderBlock :: Block -> JiraPrinter Text
renderBlock = \case
  Code lang params content -> return $ mconcat
                              [ "{code:"
                              , T.intercalate "|"
                                (renderLang lang : map renderParam params)
                              , "}\n"
                              , content
                              , "\n{code}"
                              ]
  Color colorName blocks   -> renderBlocks blocks >>= \blks -> return $ mconcat
                              [ "{color:", colorText colorName, "}\n"
                              , blks
                              , "{color}"
                              ]
  BlockQuote [Para xs]     -> return $ "bq. " <> prettyInlines xs
  BlockQuote blocks        -> renderBlocks blocks >>= \blks -> return $ mconcat
                              [ "{quote}\n"
                              , blks
                              , "\n{quote}"]
  Header lvl inlines       -> return $ mconcat
                              [ "h",  T.pack (show lvl), ". "
                              , prettyInlines inlines
                              ]
  HorizontalRule           -> return "----"
  List style items         -> listWithMarker items (styleChar style)
  NoFormat params content  -> return $ mconcat
                              [ "{noformat"
                              , if null params then mempty else renderParams params
                              , "}\n"
                              , content
                              , "{noformat}"
                              ]
  Panel params blocks     -> renderBlocks blocks >>= \blks ->
                             return $ mconcat
                             [ "{panel"
                             , if null params then mempty else T.cons ':' $ renderParams params
                             , "}\n"
                             , blks
                             , "{panel}"
                             ]
  Para inlines              -> return $ prettyInlines inlines
  Table rows                ->
    local (\st -> st { stateInTable = True }) $
      fmap T.unlines (mapM renderRow rows)

-- | Returns the ext representation of a color
colorText :: ColorName -> Text
colorText (ColorName c) = c

renderLang :: Language -> Text
renderLang (Language lang) = lang

renderParams :: [Parameter] -> Text
renderParams = T.intercalate "|" . map renderParam

renderParam :: Parameter -> Text
renderParam (Parameter key value) = key <> "=" <> value

renderRow :: Row -> JiraPrinter Text
renderRow (Row cells) = do
  rendered <- mapM renderCell cells
  let closing = if all isHeaderCell cells then " ||" else " |"
  return $ T.unwords rendered <> closing
  where
    isHeaderCell HeaderCell {} = True
    isHeaderCell BodyCell {}   = False

renderCell :: Cell -> JiraPrinter Text
renderCell cell = let (cellStart, blocks) = case cell of
                        (HeaderCell bs) -> ("|| ", bs)
                        (BodyCell bs) -> ("| ", bs)
                  in (cellStart <>) <$> renderBlocks blocks

styleChar :: ListStyle -> Char
styleChar = \case
  CircleBullets -> '*'
  SquareBullets -> '-'
  Enumeration   -> '#'

-- | Create a list using the given character as bullet item marker.
listWithMarker :: [[Block]]
               -> Char
               -> JiraPrinter Text
listWithMarker items marker =
  local (\s -> s { stateListLevel = stateListLevel s `T.snoc` marker }) $
    concatBlocks =<< mapM listItemToJira items

-- | Convert bullet or ordered list item (list of blocks) to Jira.
listItemToJira :: [Block]
               -> JiraPrinter Text
listItemToJira items = do
  contents <- renderBlocks items
  marker <- asks stateListLevel
  return $ case items of
    List{} : _ -> contents
    _          -> marker <> " " <> contents

-- | Renders a single inline item as Jira markup.
renderInline :: Inline -> Text
renderInline = \case
  Anchor name            -> "{anchor:" <> name <> "}"
  AutoLink url           -> urlText url
  Emoji icon             -> iconText icon
  Entity entity          -> "&" <> entity <> ";"
  Image params url       -> "!" <> urlText url <>
                            if null params
                            then "!"
                            else "|" <> renderParams params <> "!"
  Linebreak              -> "\n"
  Link inlines (URL url) -> "[" <> prettyInlines inlines <> "|" <> url <> "]"
  Monospaced inlines     -> "{{" <> prettyInlines inlines <> "}}"
  Space                  -> " "
  SpecialChar c          -> case c of
                              '\\' -> "\\"   -- backslash is unescapable
                              _    -> "\\" `T.snoc` c
  Str txt                -> txt
  Styled style inlines   -> renderWrapped (delimiterChar style) inlines

renderStyledSafely :: InlineStyle -> [Inline] -> Text
renderStyledSafely style =
  let delim = T.pack ['{', delimiterChar style, '}']
  in (delim <>) . (<> delim) . prettyInlines

delimiterChar :: InlineStyle -> Char
delimiterChar = \case
  Emphasis -> '_'
  Insert -> '+'
  Strong -> '*'
  Strikeout -> '-'
  Subscript -> '~'
  Superscript -> '^'

-- | Text rendering of an URL.
urlText :: URL -> Text
urlText (URL url) = url

renderWrapped :: Char -> [Inline] -> Text
renderWrapped c = T.cons c . flip T.snoc c . prettyInlines
