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
  , deleted
  , emph
  , inserted
  , linebreak
  , str
  , strong
  , whitespace
  ) where

import Control.Monad (guard, void)
import Data.Char (isLetter)
import Data.Text (pack, singleton)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Parsec

-- | Parses any inline element.
inline :: JiraParser Inline
inline = choice
  [ whitespace
  , str
  , linebreak
  , emph
  , strong
  , deleted
  , inserted
  , symbol
  ] <?> "inline"

-- | Characters with a special meaning, i.e., those used for markup.
specialChars :: String
specialChars = " \n" ++ symbolChars

-- | Special characters which can be part of a string.
symbolChars :: String
symbolChars = "_+-*|"

-- | Parses an in-paragraph newline as a @Linebreak@ element.
linebreak :: JiraParser Inline
linebreak = Linebreak <$ try (newline <* notFollowedBy' endOfPara)
  <?> "linebreak"

-- | Parses whitespace and return a @Space@ element.
whitespace :: JiraParser Inline
whitespace = Space <$ skipMany1 (char ' ') <?> "whitespace"

-- | Parses a simple, markup-less string into a @Str@ element.
str :: JiraParser Inline
str = Str . pack
  <$> (many1 (noneOf specialChars) <?> "string")
  <* updateLastStrPos

-- | Parses a special character symbol as a @Str@.
symbol :: JiraParser Inline
symbol = Str . singleton <$> do
  inTablePred <- do
    b <- stateInTable <$> getState
    return $ if b then (/= '|') else const True
  oneOf $ filter inTablePred symbolChars
  <?> "symbol"

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

-- | Parses strongly emphasized text into @Strong@.
strong :: JiraParser Inline
strong = Strong <$> ('*' `delimitingMany` inline) <?> "strong"

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
