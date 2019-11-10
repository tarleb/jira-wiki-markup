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
  ) where

import Data.Text (pack)
import Text.Jira.Markup
import Text.Jira.Parser.Core

-- | Parses any inline element.
inline :: JiraParser Inline
inline = return $ Str (pack "test")
