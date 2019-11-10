{-|
Module      : Main
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira-wiki-markup package.
-}
module Main (main) where

import Data.Text ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Text.Jira.ParserTests

-- | Run the tests for jira-wiki-markup.
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jira-wiki-markup"
  [ Text.Jira.ParserTests.tests
  ]
