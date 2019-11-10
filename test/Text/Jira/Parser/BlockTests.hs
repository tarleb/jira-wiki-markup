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

import Data.Either (isLeft)
import Data.Text ()
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?))

tests :: TestTree
tests = testGroup "Blocks"
  [ testGroup "components"
    [ testGroup "para"
      [ testCase "two lines" $
        parseJira block "one\ntwo\n" @?=
        Right (Para [Str "one", Linebreak, Str "two"])
      ]

    , testCase "ended by newline" $
      parseJira para "Hello, World\n" @?=
      Right (Para [Str "Hello,", Space, Str "World"])

    , testCase "ended by eof" $
      parseJira para "Hello, World" @?=
      Right (Para [Str "Hello,", Space, Str "World"])

    , testCase "ended by spaces and newline" $
      parseJira para "Hello, World  \n" @?=
      Right (Para [Str "Hello,", Space, Str "World"])

    , testCase "ended by blank line" $
      parseJira para "Hello\n\n" @?=
      Right (Para [Str "Hello"])
    ]

  , testGroup "Header"
    [ testCase "Level 1" $
      parseJira header "h1. Intro\n" @?=
      Right (Header 1 [Str "Intro"])

    , testCase "many spaces before title" $
      parseJira header "h2.         space\n" @?=
      Right (Header 2 [Str "space"])

    , testCase "no space after dot" $
      parseJira header "h3.hug\n" @?=
      Right (Header 3 [Str "hug"])

    , testCase "empty header" $
      parseJira header "h4.\n" @?=
      Right (Header 4 [])

    , testCase "Level 6" $
      parseJira header "h6. More\n" @?=
      Right (Header 6 [Str "More"])

    , testCase "Level 7 fails" $
      isLeft (parseJira header "h7. More\n") @? "level 7 header"

    , testCase "Level 0 fails" $
      isLeft (parseJira header "h0. More\n") @? "level 0 header"

    , testCase "leading spaces are disallowed" $
      isLeft (parseJira header " h1. nope\n") @? "leading spaces"
    ]

  , testGroup "block parser"
    [ testCase "single paragraph" $
      parseJira block "Lorem ipsum." @?=
      Right (Para [Str "Lorem", Space, Str "ipsum."])

    , testCase "para before header" $
      parseJira ((,) <$> block <*> block) "paragraph\nh1.header\n" @?=
      Right (Para [Str "paragraph"], Header 1 [Str "header"])

    , testCase "para after header" $
      parseJira ((,) <$> block <*> block) "h2.header\nparagraph\n" @?=
      Right (Header 2 [Str "header"], Para [Str "paragraph"])
    ]
  ]
