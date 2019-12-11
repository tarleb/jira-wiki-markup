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

import Data.Text (Text)
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
    ]

  , testGroup "isolated inline"
    [ testCase "SpecialChar" $
      renderInline (SpecialChar '*') @?= "\\*"

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
    ]
  ]

renderBlock' :: Block -> Text
renderBlock' = withDefault . renderBlock
