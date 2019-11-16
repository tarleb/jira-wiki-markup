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
  , ListStyle (..)
  , Row (..)
  , Cell (..)
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
  | List ListStyle [[Block]] -- ^ List
  | Para [Inline]            -- ^ Paragraph of text
  | Table [Row]              -- ^ Table
  deriving (Eq, Ord, Show)

-- | Style used for list items.
data ListStyle
  = CircleBullets            -- ^ List with round bullets
  | SquareBullets            -- ^ List with square bullets
  | Enumeration              -- ^ Enumeration, i.e., numbered items
  deriving (Eq, Ord, Show)

-- | Table row, containing an arbitrary number of cells.
newtype Row = Row { fromRow :: [Cell] }
  deriving (Eq, Ord, Show)

-- | Table cell with block content
data Cell
  = BodyCell [Block]
  | HeaderCell [Block]
  deriving (Eq, Ord, Show)
