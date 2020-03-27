{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Text.Jira.Parser.Inline
Copyright   : © 2019–2020 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki inline markup.
-}
module Text.Jira.Parser.Inline
  ( inline
    -- * Inline component parsers
  , anchor
  , autolink
  , colorInline
  , dash
  , emoji
  , entity
  , image
  , linebreak
  , link
  , monospaced
  , specialChar
  , str
  , styled
  , whitespace
    -- * Constants
  , specialChars
  ) where

import Control.Monad (guard, void)
import Data.Char (isAlphaNum, isPunctuation, ord)
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>), All (..))
#else
import Data.Monoid (All (..))
#endif
import Data.Text (append, pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Shared
import Text.Parsec

-- | Parses any inline element.
inline :: JiraParser Inline
inline = notFollowedBy' blockEnd *> choice
  [ whitespace
  , emoji
  , dash
  , autolink
  , str
  , linebreak
  , link
  , image
  , styled
  , colorInline
  , monospaced
  , anchor
  , entity
  , specialChar
  ] <?> "inline"
  where
    blockEnd = char '{' *> choice (map string blockNames) <* char '}'

-- | Characters which, depending on context, can have a special meaning.
specialChars :: String
specialChars = "_+-*^~|[]{}(!&\\:;"

-- | Parses an in-paragraph newline as a @Linebreak@ element. Both newline
-- characters and double-backslash are recognized as line-breaks.
linebreak :: JiraParser Inline
linebreak = Linebreak <$ try (
  choice [ void $ newline <* notFollowedBy' endOfPara
         , void $ string "\\\\" <* notFollowedBy' (char '\\')
         ]
  ) <?> "linebreak"

-- | Parses whitespace and return a @Space@ element.
whitespace :: JiraParser Inline
whitespace = Space <$ skipMany1 (char ' ') <?> "whitespace"

-- | Parses a simple, markup-less string into a @Str@ element.
str :: JiraParser Inline
str = Str . pack . mconcat
  <$> many1 (alphaNums <|> otherNonSpecialChars)
  <?> "string"
  where
    nonStrChars = " \n" ++ specialChars
    alphaNums = many1 alphaNum <* updateLastStrPos
    otherNonSpecialChars = many1 . satisfy $ \c ->
      not (isAlphaNum c || c `elem` nonStrChars)

-- | Parses an HTML entity into an @'Entity'@ element.
entity :: JiraParser Inline
entity = Entity . pack
  <$> try (char '&' *> (numerical <|> named) <* char ';')
  where
    numerical = (:) <$> char '#' <*> many1 digit
    named = many1 letter

-- | Parses textual representation of an icon into an @'Emoji'@ element.
emoji :: JiraParser Inline
emoji = Emoji <$> icon <* notFollowedBy' letter <?> "emoji"

-- | Parses ASCII representation of en-dash or em-dash.
dash :: JiraParser Inline
dash = try $ do
  guard =<< notAfterString
  _ <- string "--"
  choice [ Str "—" <$ char '-'   -- em dash
         , pure (Str "–")          -- en dash
         ] <* lookAhead (void (char ' ') <|> eof)

-- | Parses a special character symbol as a @Str@.
specialChar :: JiraParser Inline
specialChar = SpecialChar <$> (escapedChar <|> plainSpecialChar)
  <?> "special char"
  where
    escapedChar = try (char '\\' *> satisfy isPunctuation)

    plainSpecialChar = do
      inTablePred <- do
        b <- stateInTable <$> getState
        return $ if b then All . (/= '|') else mempty
      inLinkPred  <- do
        b <- stateInLink  <$> getState
        return $ if b then All . (`notElem` ("]|\n" :: String)) else mempty
      oneOf $ filter (getAll . (inTablePred <> inLinkPred)) specialChars


--
-- Anchors, links and images
--

-- | Parses an anchor into an @Anchor@ element.
anchor :: JiraParser Inline
anchor = Anchor . pack . filter (/= ' ')
  <$> try (string "{anchor:" *> noneOf "\n" `manyTill` char '}')

-- | Parse image into an @Image@ element.
image :: JiraParser Inline
image = try $ do
  -- does not use @url@, as is may contain relative locations.
  src <- char '!' *> (URL . pack <$> many1 (noneOf "\r\t\n|]!"))
  params <- option [] (char '|' *> (thumbnail <|> imgParams `sepBy` comma))
  _ <- char '!'
  return $ Image params src
  where
    thumbnail = [Parameter "thumbnail" ""] <$ try (string "thumbnail")
    imgParams = try (Parameter <$> key <*> (char '=' *> value))
    key       = pack <$> many1 (noneOf ",\"'\t\n\r |{}=!")
    value     = pack <$> many1 (noneOf ",\"'\n\r|{}=!")
    comma     = char ',' *> skipSpaces

-- | Parse link into a @Link@ element.
link :: JiraParser Inline
link = try $ do
  guard . not . stateInLink =<< getState
  withStateFlag (\b st -> st { stateInLink = b }) $ do
    _ <- char '['
    alias   <- option [] $ try (many inline <* char '|')
    linkUrl <- email <|> url
    _ <- char ']'
    return $ Link alias linkUrl

autolink :: JiraParser Inline
autolink = AutoLink <$> (email <|> url) <?> "email or other URL"

-- | Parse a URL with scheme @file@, @ftp@, @http@, @https@, @irc@, @nntp@, or
-- @news@.
url :: JiraParser URL
url = try $ do
  urlScheme <- scheme
  sep <- pack <$> string "://"
  rest <- pack <$> many urlChar
  return $ URL (urlScheme `append` sep `append` rest)
  where
    scheme = do
      first <- letter
      case first of
        'f' -> ("file" <$ string "ile") <|> ("ftp" <$ string "tp")
        'h' -> string "ttp" *> option "http" ("https" <$ char 's')
        'i' -> "irc" <$ string "rc"
        'n' -> ("nntp" <$ string "ntp") <|> ("news" <$ string "ews")
        _   -> fail "not looking at a known scheme"

-- | Parses an E-mail URL.
email :: JiraParser URL
email = URL . pack <$> try
  ((++) <$> string "mailto:" <*> many1 urlChar)

-- | Parses a character which is allowed in URLs
urlChar :: JiraParser Char
urlChar = satisfy $ \c ->
  c `notElem` ("|]" :: String) && ord c >= 32 && ord c <= 127

--
-- Color
--

-- | Text in a different color.
colorInline :: JiraParser Inline
colorInline = try $ do
  name <- string "{color:" *> (colorName <|> colorCode) <* char '}'
  content <- inline `manyTill` try (string "{color}")
  return $ ColorInline (ColorName $ pack name) content
  where
    colorName = many1 letter
    colorCode = (:) <$> option '#' (char '#') <*> count 6 digit

--
-- Markup
--

-- | Parses styled text
styled :: JiraParser Inline
styled = (simpleStyled <|> forceStyled) <?> "styled text"
  where
    simpleStyled = try $ do
      styleChar <- lookAhead $ oneOf "-_+*~^"
      content   <- styleChar `delimitingMany` inline
      let style = delimiterStyle styleChar
      return $ Styled style content

    forceStyled = try $ do
      styleChar <- char '{' *> oneOf "-_+*~^" <* char '}'
      let closing = try $ string ['{', styleChar, '}']
      let style   = delimiterStyle styleChar
      content   <- manyTill inline closing
      return $ Styled style content

-- | Returns the markup kind from the delimiting markup character.
delimiterStyle :: Char -> InlineStyle
delimiterStyle = \case
  '*' -> Strong
  '+' -> Insert
  '-' -> Strikeout
  '^' -> Superscript
  '_' -> Emphasis
  '~' -> Subscript
  c   -> error ("Unknown delimiter character: " ++ [c])

-- | Parses monospaced text into @Monospaced@.
monospaced :: JiraParser Inline
monospaced = Monospaced
  <$> enclosed (try $ string "{{") (try $ string "}}") inline
  <?> "monospaced"

--
-- Helpers
--

-- | Parse text delimited by a character.
delimitingMany :: Char -> JiraParser a -> JiraParser [a]
delimitingMany c = enclosed (char c) (char c)

enclosed :: JiraParser opening -> JiraParser closing
         -> JiraParser a
         -> JiraParser [a]
enclosed opening closing parser = try $ do
  guard =<< notAfterString
  opening *> notFollowedBy space *> manyTill parser closing'
  where
    closing' = try $ closing <* lookAhead wordBoundary
    wordBoundary = void (satisfy (not . isAlphaNum)) <|> eof
