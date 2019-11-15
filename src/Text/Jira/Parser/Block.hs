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
  , list
  , para
  ) where

import Control.Monad (guard, void)
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
  , list
  , para
  ] <* skipWhitespace

-- | Parses a paragraph into a @Para@.
para :: JiraParser Block
para = (<?> "para") . try $
  let dropTrailingWhitespace = dropWhileEnd (`elem` [Linebreak, Space])
  in Para . dropTrailingWhitespace <$> many1 inline <* (eof <|> void newline)

-- | Parses a header line into a @Header@.
header :: JiraParser Block
header = (<?> "header") . try $ do
  level <- digitToInt <$> (char 'h' *> oneOf "123456" <* char '.')
  content <- skipMany (char ' ') *> inline `manyTill` newline
  return $ Header level content

-- | Parses a list into @List@.
list :: JiraParser Block
list = (<?> "list") . try $ do
  guard . not . stateInList =<< getState
  -- withStateFlag (\b st -> st { stateInList = b }) $
  listAtDepth 0
  where
    listAtDepth :: Int -> JiraParser Block
    listAtDepth depth = try $ atDepth depth *> listAtDepth' depth

    listAtDepth' :: Int -> JiraParser Block
    listAtDepth' depth = try $ do
      bulletChar <- oneOf "*-#"
      first <- firstItemAtDepth depth
      rest  <- many (try $ listItemAtDepth depth (char bulletChar))
      return $ List (style bulletChar) (first:rest)

    style :: Char -> ListStyle
    style c = case c of
      '-' -> SquareBullets
      '*' -> CircleBullets
      '#' -> Enumeration
      _   -> error ("the impossible happened: unknown style for bullet " ++ [c])

    atDepth :: Int -> JiraParser ()
    atDepth depth = try . void $ count depth anyBulletMarker

    firstItemAtDepth :: Int -> JiraParser [Block]
    firstItemAtDepth depth = try $ listContent (depth + 1) <|>
      do
        blocks <- nonListContent
        nestedLists <- try . many $ listAtDepth (depth + 1)
        return $ blocks ++ nestedLists

    listItemAtDepth :: Int -> JiraParser Char -> JiraParser [Block]
    listItemAtDepth depth bulletChar = atDepth depth *>
        try (bulletChar *> nonListContent) <|>
        try (anyBulletMarker *> listContent depth)

    listContent :: Int -> JiraParser [Block]
    listContent depth = do
        first <- listAtDepth' depth
        rest <- many (listAtDepth depth)
        return (first : rest)

    anyBulletMarker :: JiraParser ListStyle
    anyBulletMarker = circleMarker <|> squareMarker <|> enumerationMarker
      where
        circleMarker = CircleBullets <$ char '*'
        squareMarker = SquareBullets <$ char '-'
        enumerationMarker = Enumeration <$ char '#'

    nonListContent :: JiraParser [Block]
    nonListContent = try $
      let nonListBlock = notFollowedBy' (many1 (oneOf "#-*")) *> block
      in char ' ' *> do
        first <- block
        rest  <- many nonListBlock
        return (first : rest)

-- | Skip whitespace till we reach the next block
skipWhitespace :: JiraParser ()
skipWhitespace = optional $ do
  guard . not . stateInList =<< getState
  skipMany blankline
