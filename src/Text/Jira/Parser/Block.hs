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
  , header
  , para
  ) where

import Data.List (dropWhileEnd)
import Data.Char (digitToInt)
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec

-- | Parses any block element.
block :: JiraParser Block
block = choice
  [ header
  , para
  ]

-- | Parses a paragraph into a @Para@.
para :: JiraParser Block
para = (<?> "paragraph") . try $
  Para . dropWhileEnd (`elem` [Linebreak, Space])
       <$> many1 inline <* optional newline

-- | Parses a header line into a @Header@.
header :: JiraParser Block
header = (<?> "header") . try $ do
  level <- digitToInt <$> (char 'h' *> oneOf "123456" <* char '.')
  content <- skipMany (char ' ') *> inline `manyTill` newline
  return $ Header level content
