{-|
Module      : Text.Jira.ParserTests
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki parser.
-}
module Text.Jira.ParserTests (tests) where

import Data.Text ()
import Test.Tasty (TestTree, testGroup)
import qualified Text.Jira.Parser.BlockTests
import qualified Text.Jira.Parser.InlineTests

tests :: TestTree
tests = testGroup "Parser"
  [ Text.Jira.Parser.InlineTests.tests
  , Text.Jira.Parser.BlockTests.tests
  ]
