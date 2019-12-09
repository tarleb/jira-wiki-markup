{-|
Module      : Text.Jira.Parser.Inline
Copyright   : © 2019 Albert Krewinkel
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
  , deleted
  , emph
  , entity
  , inserted
  , image
  , linebreak
  , link
  , monospaced
  , str
  , strong
  , subscript
  , superscript
  , symbol
  , whitespace
  ) where

import Control.Monad (guard, void)
import Data.Char (isLetter)
import Data.Monoid ((<>), All (..))
import Data.Text (pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Parsec

-- | Parses any inline element.
inline :: JiraParser Inline
inline = notFollowedBy' blockEnd *> choice
  [ whitespace
  , str
  , linebreak
  , link
  , image
  , emph
  , strong
  , subscript
  , superscript
  , deleted
  , inserted
  , monospaced
  , anchor
  , entity
  , symbol
  ] <?> "inline"
  where
    blockEnd = char '{' *> choice (map string blockNames) <* char '}'

-- | Characters with a special meaning, i.e., those used for markup.
specialChars :: String
specialChars = " \n" ++ symbolChars

-- | Special characters which can be part of a string.
symbolChars :: String
symbolChars = "_+-*^~|[]{}!&\\"

-- | Parses an in-paragraph newline as a @Linebreak@ element.
linebreak :: JiraParser Inline
linebreak = Linebreak <$ try (newline <* notFollowedBy' endOfPara)
  <?> "linebreak"

-- | Parses whitespace and return a @Space@ element.
whitespace :: JiraParser Inline
whitespace = Space <$ skipMany1 (char ' ') <?> "whitespace"

-- | Parses a simple, markup-less string into a @Str@ element.
str :: JiraParser Inline
str = Str . pack <$> (alphaNums <|> otherNonSpecialChars) <?> "string"
  where
    alphaNums = many1 alphaNum <* updateLastStrPos
    otherNonSpecialChars = many1 (noneOf specialChars)

-- | Parses an HTML entity into an @'Entity'@ element.
entity :: JiraParser Inline
entity = Entity . pack
  <$> try (char '&' *> (numerical <|> named) <* char ';')
  where
    numerical = (:) <$> char '#' <*> many1 digit
    named = many1 letter

-- | Parses a special character symbol as a @Str@.
symbol :: JiraParser Inline
symbol = SpecialChar <$> (escapedChar <|> symbolChar)
  <?> "symbol"
  where
    escapedChar = try (char '\\' *> oneOf symbolChars)

    symbolChar = do
      inTablePred <- do
        b <- stateInTable <$> getState
        return $ if b then All . (/= '|') else mempty
      inLinkPred  <- do
        b <- stateInLink  <$> getState
        return $ if b then All . (`notElem` ("]|\n" :: String)) else mempty
      oneOf $ filter (getAll . (inTablePred <> inLinkPred)) symbolChars


--
-- Anchors, links and images
--

-- | Parses an anchor into an @Anchor@ element.
anchor :: JiraParser Inline
anchor = Anchor . pack . filter (/= ' ')
  <$> try (string "{anchor:" *> noneOf "\n" `manyTill` char '}')

-- | Parse image into an @Image@ element.
image :: JiraParser Inline
image = fmap (Image . URL . pack) . try $
  char '!' *> noneOf "\r\t\n" `manyTill` char '!'

-- | Parse link into a @Link@ element.
link :: JiraParser Inline
link = try $ do
  guard . not . stateInLink =<< getState
  withStateFlag (\b st -> st { stateInLink = b }) $ do
    _ <- char '['
    alias <- option [] $ try (many inline <* char '|')
    url   <- URL . pack <$> many1 (noneOf "|] \n")
    _ <- char ']'
    return $ Link alias url

--
-- Markup
--
-- | Parses deleted text into @Deleted@.
deleted :: JiraParser Inline
deleted = Deleted <$> ('-' `delimitingMany` inline) <?> "deleted"

-- | Parses emphasized text into @Emph@.
emph :: JiraParser Inline
emph = Emph <$> ('_' `delimitingMany` inline) <?> "emphasis"

-- | Parses inserted text into @Inserted@.
inserted :: JiraParser Inline
inserted = Inserted <$> ('+' `delimitingMany` inline) <?> "inserted"

-- | Parses monospaced text into @Monospaced@.
monospaced :: JiraParser Inline
monospaced = Monospaced
  <$> enclosed (try $ string "{{") (try $ string "}}") inline
  <?> "monospaced"

-- | Parses strongly emphasized text into @Strong@.
strong :: JiraParser Inline
strong = Strong <$> ('*' `delimitingMany` inline) <?> "strong"

-- | Parses subscript text into @Subscript@.
subscript :: JiraParser Inline
subscript = Subscript <$> ('~' `delimitingMany` inline) <?> "subscript"

-- | Parses superscript text into @Superscript@.
superscript :: JiraParser Inline
superscript = Superscript <$> ('^' `delimitingMany` inline) <?> "superscript"

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
    wordBoundary = void (satisfy (not . isLetter)) <|> eof
