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
  ) where

import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec (many1)

-- | Parses any block element.
block :: JiraParser Block
block = Para <$> many1 inline
