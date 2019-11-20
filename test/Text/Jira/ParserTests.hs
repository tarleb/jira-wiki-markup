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
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Jira.Parser
import qualified Text.Jira.Parser.BlockTests
import qualified Text.Jira.Parser.InlineTests

tests :: TestTree
tests = testGroup "Parser"
  [ Text.Jira.Parser.InlineTests.tests
  , Text.Jira.Parser.BlockTests.tests
  , testGroup "doc"
    [ testCase "empty document" $
      parse "" @?=
      Right (Doc [])

    , testCase "simple document" $
      parse "h1. test\nThis is ok." @?=
      Right (Doc [ Header 1 [Str "test"]
                 , Para [Str "This", Space, Str "is", Space, Str "ok."]])
    ]
  ]
