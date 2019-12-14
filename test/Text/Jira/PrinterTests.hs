{-|
Module      : Text.Jira.PrinterTests
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki printer.
-}
module Text.Jira.PrinterTests (tests) where

import Prelude hiding (unlines)
import Data.Text (Text, unlines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Jira.Markup
import Text.Jira.Printer

tests :: TestTree
tests = testGroup "Printer"
  [ testGroup "doc"
    [ testCase "empty document" $
      pretty (Doc []) @?= ""
    ]

  , testGroup "blocks"
    [ testCase "simple paragraph" $
      let para = Para [Str "Hello,", Space, Str "World!"]
      in renderBlock' para @?= "Hello, World!"

    , testGroup "header"
      [ testCase "simple" $
        let header = Header 1 [Str "test", Space, Str "header"]
        in renderBlock' header @?= "h1. test header"

      , testCase "header" $
        let header = Header 5 [Str "test", Space, Str "header"]
        in renderBlock' header @?= "h5. test header"
      ]

    , testCase "horizontal rule" $
      renderBlock' HorizontalRule @?= "----"

    , testCase "color" $
      renderBlock' (Color (ColorName "blue") [Para [Str "yabadee"]]) @?=
      "{color:blue}\nyabadee\n{color}"

    , testGroup "list"
      [ testCase "simple list" $
        let list = List SquareBullets [ [Para [Str "first"]]
                                      , [Para [Str "second"]]
                                      , [Para [Str "third"]]
                                      ]
        in renderBlock' list @?= unlines
           [ "- first"
           , "- second"
           , "- third"
           ]

      , testCase "nested list" $
        let list = List CircleBullets
                   [ [Para [Str "first"]]
                   , [ List Enumeration   [ [Para [Str "second-1"]]
                                          , [Para [Str "second-2"]]]]
                   , [ Para [Str "third"]
                     , List CircleBullets [ [Para [Str "third-1"]]
                                          , [Para [Str "third-2"]]]]
                   ]
        in renderBlock' list @?= unlines
           [ "* first"
           , "*# second-1"
           , "*# second-2"
           , "* third"
           , "** third-1"
           , "** third-2"
           ]
      ]

    , testCase "table" $
      let headerRow = Row [ HeaderCell [Para [Str "one"]]
                          , HeaderCell [Para [Str "two"]]
                          ]
          bodyRow = Row [ BodyCell [Para [Str "1"]]
                        , BodyCell [Para [Str "2"]]
                        ]
          table = Table [headerRow, bodyRow]
      in renderBlock' table @?= "|| one || two ||\n| 1 | 2 |\n"

    , testCase "para after table" $
      let table = Table [Row [BodyCell [Para [Str "boring"]]]]
          para = Para [Str "after", Space, Str "table"]
      in prettyBlocks [table, para] @?= "| boring |\n\nafter table\n"

    , testCase "para after list" $
      let list = List Enumeration [[Para [Str "boring"]]]
          para = Para [Str "after", Space, Str "table"]
      in prettyBlocks [list, para] @?= "# boring\n\nafter table\n"
    ]

  , testGroup "isolated inline"
    [ testCase "SpecialChar" $
      renderInline (SpecialChar '*') @?= "\\*"

    , testCase "AutoLink" $
      renderInline (AutoLink (URL "https://example.org")) @?=
      "https://example.org"

    , testCase "Emoji" $
      renderInline (Emoji IconSmiling) @?= ":D"

    , testCase "Styled Emphasis" $
      renderInline (Styled Emphasis [Str "Hello,", Space, Str "World!"]) @?=
      "_Hello, World!_"

    , testCase "Styled Strong" $
      renderInline (Styled Strong [Str "Hello,", Space, Str "World!"]) @?=
      "*Hello, World!*"
    ]

  , testGroup "combined inlines"
    [ testCase "special char between words" $
      prettyInlines [Str "easy", SpecialChar '-', Str "peasy"] @?=
      "easy-peasy"

    , testCase "special char before word" $
      prettyInlines [SpecialChar '*', Str "star"] @?=
      "\\*star"

    , testCase "markup within word" $
      prettyInlines [Str "H", Styled Subscript [Str "2"], Str "O"] @?=
      "H{~}2{~}O"

    , testCase "markup followed by punctuation" $
      prettyInlines [Styled Emphasis [Str "Word"], Str "."] @?=
      "_Word_."
    ]
  ]

renderBlock' :: Block -> Text
renderBlock' = withDefault . renderBlock
