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
  (
  -- * Jira parser and state
    JiraParser
  , ParserState (..)
  , defaultState
  , parseJira
  , withStateFlag
  -- * Parsing helpers
  , endOfPara
  , notFollowedBy'
  , blankline
  , skipSpaces
  ) where

import Control.Monad (join, void)
import Data.Text (Text)
import Text.Parsec

-- | Jira Parsec parser
type JiraParser = Parsec Text ParserState

-- | Parser state used to keep track of various parameteres.
data ParserState = ParserState
  { stateInList      :: Bool            -- ^ whether the parser is within a list
  , stateInTable     :: Bool            -- ^ whether the parser is within a table
  }

-- | Default parser state (i.e., start state)
defaultState :: ParserState
defaultState = ParserState
  { stateInList      = False
  , stateInTable     = False
  }

-- | Set a flag in the parser to @True@ before running a parser, then
-- set the flag's value to @False@.
withStateFlag :: (Bool -> ParserState -> ParserState)
              -> JiraParser a
              -> JiraParser a
withStateFlag flagSetter parser = try $
  let setFlag = modifyState . flagSetter
  in setFlag True *> parser <* setFlag False

-- | Parses a string with the given Jira parser.
parseJira :: JiraParser a -> Text -> Either ParseError a
parseJira parser = runParser parser defaultState ""

-- | Skip zero or more space chars.
skipSpaces :: JiraParser ()
skipSpaces = skipMany (char ' ')

-- | Parses an empty line, i.e., a line with no chars or whitespace only.
blankline :: JiraParser ()
blankline = try $ skipSpaces *> void newline

-- | Succeeds if the parser is looking at the end of a paragraph.
endOfPara :: JiraParser ()
endOfPara = eof
  <|> lookAhead blankline
  <|> lookAhead headerStart
  <|> lookAhead listItemStart
  <|> lookAhead tableStart
  <|> lookAhead codeStart
  where
    headerStart   = void $ try $ char 'h' *> oneOf "123456" <* char '.'
    listItemStart = void $ many1 (oneOf "#*-") *> char ' '
    tableStart    = void $ skipSpaces *> many1 (char '|') *> char ' '
    codeStart     = void $ string "{code"

-- | Variant of parsec's @notFollowedBy@ function which properly fails even if
-- the given parser does not consume any input (like @eof@ does).
notFollowedBy' :: Show a => JiraParser a -> JiraParser ()
notFollowedBy' p =
  let failIfSucceeds = unexpected . show <$> try p
      unitParser = return (return ())
  in try $ join (failIfSucceeds <|> unitParser)
