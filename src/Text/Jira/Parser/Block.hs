{-|
Module      : Text.Jira.Parser.Block
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki blocks.
-}
module Text.Jira.Parser.Block
  ( block
    -- * Parsers for block types
  , code
  , header
  , list
  , para
  , table
  ) where

import Control.Monad (guard, void, when)
import Data.List (dropWhileEnd)
import Data.Char (digitToInt)
import Data.Text (pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec

-- | Parses any block element.
block :: JiraParser Block
block = choice
  [ header
  , list
  , table
  , code
  , para
  ] <* skipWhitespace

-- | Parses a paragraph into a @Para@.
para :: JiraParser Block
para = (<?> "para") . try $ do
  let dropTrailingWhitespace = dropWhileEnd (`elem` [Linebreak, Space])
  isInList <- stateInList <$> getState
  when isInList $
    notFollowedBy' blankline
  Para . dropTrailingWhitespace <$> many1 inline

-- | Parses a header line into a @Header@.
header :: JiraParser Block
header = (<?> "header") . try $ do
  level <- digitToInt <$> (char 'h' *> oneOf "123456" <* char '.')
  content <- skipMany (char ' ') *> inline `manyTill` newline
  return $ Header level content

-- | Parses a list into @List@.
list :: JiraParser Block
list = (<?> "list") . try $ do
  guard . not . stateInList =<< getState
  withStateFlag (\b st -> st { stateInList = b }) $
    listAtDepth 0
  where
    listAtDepth :: Int -> JiraParser Block
    listAtDepth depth = try $ atDepth depth *> listAtDepth' depth

    listAtDepth' :: Int -> JiraParser Block
    listAtDepth' depth = try $ do
      bulletChar <- anyBulletMarker
      first <- firstItemAtDepth depth
      rest  <- many (try $ listItemAtDepth depth (char bulletChar))
      return $ List (style bulletChar) (first:rest)

    style :: Char -> ListStyle
    style c = case c of
      '-' -> SquareBullets
      '*' -> CircleBullets
      '#' -> Enumeration
      _   -> error ("the impossible happened: unknown style for bullet " ++ [c])

    atDepth :: Int -> JiraParser ()
    atDepth depth = try . void $ count depth anyBulletMarker

    firstItemAtDepth :: Int -> JiraParser [Block]
    firstItemAtDepth depth = try $ listContent (depth + 1) <|>
      do
        blocks <- nonListContent
        nestedLists <- try . many $ listAtDepth (depth + 1)
        return $ blocks ++ nestedLists

    listItemAtDepth :: Int -> JiraParser Char -> JiraParser [Block]
    listItemAtDepth depth bulletChar = atDepth depth *>
        try (bulletChar *> nonListContent) <|>
        try (anyBulletMarker *> listContent depth)

    listContent :: Int -> JiraParser [Block]
    listContent depth = do
        first <- listAtDepth' depth
        rest <- many (listAtDepth depth)
        return (first : rest)

    anyBulletMarker :: JiraParser Char
    anyBulletMarker = oneOf "*-#"

    nonListContent :: JiraParser [Block]
    nonListContent = try $
      let nonListBlock = notFollowedBy' (many1 (oneOf "#-*")) *> block
      in char ' ' *> do
        first <- block
        rest  <- many nonListBlock
        return (first : rest)

-- | Parses a table into a @Table@ element.
table :: JiraParser Block
table = do
  guard . not . stateInTable =<< getState
  withStateFlag (\b st -> st { stateInTable = b }) $
    Table <$> many1 row

-- | Parses a table row.
row :: JiraParser Row
row = fmap Row . try $
  many1 cell <* optional (skipMany (oneOf " |") *> newline)

-- | Parses a table cell.
cell :: JiraParser Cell
cell = try $ do
  mkCell <- cellStart
  bs     <- many1 block
  return $ mkCell bs

-- | Parses the beginning of a table cell and returns a function which
-- constructs a cell of the appropriate type when given the cell's content.
cellStart :: JiraParser ([Block] -> Cell)
cellStart = try
  $  skipSpaces
  *> char '|'
  *> option BodyCell (HeaderCell <$ many1 (char '|'))
  <* skipSpaces
  <* notFollowedBy' newline

-- | Parses a code block into a @Code@ element.
code :: JiraParser Block
code = try $ do
  (lang, params) <- string "{code" *> parameters <* char '}' <* blankline
  content <- anyChar `manyTill` try (string "{code}" *> skipSpaces *> newline)
  return $ Code lang params (pack content)

-- | Parses a set of panel parameters
parameters :: JiraParser (Language, [Parameter])
parameters = option (defaultLanguage, []) $ do
  _      <- char ':'
  lang   <- option defaultLanguage (try language)
  params <- try (Parameter <$> key <*> (char '=' *> value)) `sepBy` pipe
  return (lang, params)
  where
    defaultLanguage = Language (pack "java")
    pipe     = char '|'
    key      = pack <$> many1 (noneOf "\"'\t\n\r |{}=")
    value    = pack <$> many1 (noneOf "\"'\n\r|{}=")
    language = Language <$> key <* (pipe <|> lookAhead (char '}'))

-- | Skip whitespace till we reach the next block
skipWhitespace :: JiraParser ()
skipWhitespace = optional $ do
  isInList  <- stateInList  <$> getState
  isInTable <- stateInTable <$> getState
  case (isInList, isInTable) of
    (True, _) -> blankline
    (_, True) -> skipSpaces
    _         -> skipMany blankline
