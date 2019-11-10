{-|
Module      : Text.Jira.Parser.Core
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Core components of the Jira wiki markup parser.
-}
module Text.Jira.Parser.Core
  ( JiraParser
  , ParserState (..)
  , defaultState
  ) where

import Data.Text (Text)
import Text.Parsec (Parsec)

-- | Jira Parsec parser
type JiraParser = Parsec Text ParserState

-- | Parser state used to keep track of various parameteres.
data ParserState = ParserState

-- | Default parser state (i.e., start state)
defaultState :: ParserState
defaultState = ParserState
