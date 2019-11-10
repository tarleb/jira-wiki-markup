{-|
Module      : Text.Jira.Markup
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Jira markup types.
-}
module Text.Jira.Markup
  ( Block (..)
  , Inline (..)
  ) where

import Data.Text (Text)

-- | Inline Jira markup elements.
data Inline
  = Linebreak                -- ^ hard linebreak
  | Str Text                 -- ^ simple, markup-less string
  | Space                    -- ^ space between words
  deriving (Eq, Ord, Show)

-- | Blocks of text.
data Block
  = Header Int [Inline]      -- ^ Header with level and text
  | Para [Inline]            -- ^ Paragraph of text
  deriving (Eq, Ord, Show)

