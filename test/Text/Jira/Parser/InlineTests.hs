{-|
Module      : Text.Jira.Parser.InlineTests
Copyright   : © 2019 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki inline markup parsers.
-}
module Text.Jira.Parser.InlineTests (tests) where

import Data.Either (isLeft)
import Data.Text ()
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec (many1)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?))

tests :: TestTree
tests = testGroup "Inline"
  [ testGroup "components"
    [ testGroup "str"
      [ testCase "simple word" $
        parseJira str "word" @?= Right (Str "word")

      , testCase "word with comma" $
        parseJira str "comma," @?= Right (Str "comma,")

      , testCase "umlauts" $
        parseJira str "äéíöüßðå" @?= Right (Str "äéíöüßðå")

      , testCase "space fails" $
        isLeft (parseJira str " ") @?
        "str should only be parsed into Space"
      ]

    , testGroup "whitespace"
      [ testCase "space" $
        parseJira whitespace " " @?= Right Space

      , testCase "tab" $
        isLeft (parseJira whitespace "\t") @?
        "TAB is not considered whitespace"

      , testCase "nonbreaking space fails" $
        isLeft (parseJira whitespace "\160") @?
        "NBSP is not considered whitespace"

      , testCase "zero width space fails" $
        isLeft (parseJira whitespace "\8203") @?
        "ZWSP is not considered whitespace"

      , testCase "newline fails" $
        isLeft (parseJira whitespace "\n") @?
        "newline is not considered whitespace"
      ]

    , testGroup "linebreak"
      [ testCase "linebreak before text" $
        parseJira linebreak "\na" @?=
        Right Linebreak

      , testCase "linebreak at eof fails" $
        isLeft (parseJira linebreak "\n") @? "newline before eof"

      , testCase "linebreak before blank line fails" $
        isLeft (parseJira linebreak "\n\n") @? "newline before blank line"

      , testCase "linebreak before list fails" $
        isLeft (parseJira linebreak "\n\n") @? "newline before list"

      , testCase "linebreak before header fails" $
        isLeft (parseJira linebreak "\nh1.foo\n") @? "newline before header"
      ]
    ]

  , testGroup "inline parser"
    [ testCase "simple sentence" $
      parseJira (many1 inline) "Hello, World!" @?=
      Right [Str "Hello,", Space, Str "World!"]
    ]
  ]
