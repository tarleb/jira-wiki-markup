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
  , linebreak
  , str
  , whitespace
  ) where

import Data.Text (pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Parsec

-- | Parses any inline element.
inline :: JiraParser Inline
inline = choice
  [ whitespace
  , str
  , linebreak
  ] <?> "inline"

-- | Characters with a special meaning, i.e., those used for markup.
specialChars :: String
specialChars = " \n"

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
