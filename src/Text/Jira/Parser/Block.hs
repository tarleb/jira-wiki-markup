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
  ]

-- | Parses a paragraph into a @Para@.
para :: JiraParser Block
para = (<?> "paragraph") . try $
  Para . dropWhileEnd (`elem` [Linebreak, Space])
       <$> many1 inline <* endOfPara

-- | Parses a header line into a @Header@.
header :: JiraParser Block
header = (<?> "header") . try $ do
  level <- digitToInt <$> (char 'h' *> oneOf "123456" <* char '.')
  content <- skipMany (char ' ') *> inline `manyTill` newline
  return $ Header level content

-- | Parses a list into @List@.
list :: JiraParser Block
list = do
  guard . not . stateInList =<< getState
  withStateFlag (\b st -> st { stateInList = b }) $
    listAtDepth 0
  where
    listAtDepth :: Int -> JiraParser Block
    listAtDepth depth = try $ atDepth depth *> listAtDepth' depth

    listAtDepth' :: Int -> JiraParser Block
    listAtDepth' depth = try $ do
      style <- anyBulletMarker
      first <- firstItemAtDepth depth style
      rest  <- many (listItemAtDepth depth style)
      return $ List style (first:rest)

    atDepth :: Int -> JiraParser ()
    atDepth depth = try . void $ count depth anyBulletMarker

    firstItemAtDepth :: Int -> ListStyle -> JiraParser [Block]
    firstItemAtDepth depth _style = try $ ((:[]) <$> listAtDepth' (depth + 1)) <|>
      do
        blocks <- char ' ' *> ((:) <$> block <*> many block')
        nestedList <- option [] (try ((:[]) <$> listAtDepth (depth + 1)))
        return $ blocks ++ nestedList

    block' :: JiraParser Block
    block' = notFollowedBy' (try $ optional newline *> many1 (oneOf "#-*")) *> block

    listItemAtDepth :: Int -> ListStyle -> JiraParser [Block]
    listItemAtDepth depth style = try $ do
      atDepth depth *>
        (listItem style <|> ((:[]) <$> (anyBulletMarker *> listAtDepth' (depth + 1))))

    anyBulletMarker :: JiraParser ListStyle
    anyBulletMarker = circleMarker <|> squareMarker <|> enumerationMarker
      where
        circleMarker = CircleBullets <$ char '*'
        squareMarker = SquareBullets <$ char '-'
        enumerationMarker = Enumeration <$ char '#'

    listItem :: ListStyle -> JiraParser [Block]
    listItem style = try $
      let bulletChar = case style of
            CircleBullets -> '*'
            SquareBullets -> '-'
            Enumeration   -> '#'
      in char bulletChar *> char ' ' *> many block

