{-|
Module      : Text.Jira.Parser
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parse Jira wiki markup.
-}
module Text.Jira.Parser
  ( parse
  , module Text.Jira.Markup
  , module Text.Jira.Parser.Core
  , module Text.Jira.Parser.Inline
  , module Text.Jira.Parser.Block
  ) where

import Data.Text (Text)
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec hiding (parse)

-- | Parses a document into a list of blocks.
parse :: Text -> Either ParseError [Block]
parse = parseJira (many1 block)
