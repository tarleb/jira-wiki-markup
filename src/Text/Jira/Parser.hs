{-# LANGUAGE LambdaCase #-}
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
  , doc
  , plainText
  , module Text.Jira.Markup
  , module Text.Jira.Parser.Core
  , module Text.Jira.Parser.Inline
  , module Text.Jira.Parser.Block
  ) where

import Data.Text (Text, append)
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec hiding (parse)

-- | Parses a document into a Jira AST.
parse :: Text -> Either ParseError Doc
parse = parseJira doc

-- | Parses a list of jira blocks into a @'Doc'@ element.
doc :: JiraParser Doc
doc = Doc <$> many block <?> "doc"

-- | Parses into an @'Inline'@ elements which represent plain text. The result
-- contains of any number of @'Str'@, @'SpecialChar'@, or @'Space'@ elements.
--
-- This parser can be used to convert un-escaped strings into proper Jira markup
-- elements.
plainText :: Text -> Either ParseError [Inline]
plainText = parseJira (normalizeInlines <$> many plainInlineParser)
  where
    plainInlineParser :: JiraParser Inline
    plainInlineParser = choice
      [ whitespace
      , escapedEmoji
      , str
      , symbol
      ] <?> "text-only inline"

    escapedEmoji = do
      emojiText <- emoji >>= \case
                     Emoji icon -> return $ iconText icon
                     _          -> fail "should have yielded an emoji"
      -- escaped emoji text to prevent rendering
      return . Str $ "\\" `append` emojiText
