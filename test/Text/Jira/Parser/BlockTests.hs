{-|
Module      : Text.Jira.Parser.BlockTests
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for jira wiki block parsers.
-}
module Text.Jira.Parser.BlockTests (tests) where

import Data.Text ()
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Blocks"
  [ testGroup "components"
    [ testGroup "paragraph"
      [ testCase "two lines" $
        parseJira block "one\ntwo\n" @?=
        Right (Para [Str "one", Linebreak, Str "two"])
      ]
    ]

  , testGroup "block parser"
    [ testCase "single paragraph" $
      parseJira block "Lorem ipsum." @?=
      Right (Para [Str "Lorem", Space, Str "ipsum."])
    ]
  ]
