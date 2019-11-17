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

    , testCase "deleted" $
      parseJira deleted "-far-fetched-" @?=
      Right (Deleted [Str "far", Str "-", Str "fetched"])

    , testGroup "emph"
      [ testCase "single word" $
        parseJira emph "_single_" @?= Right (Emph [Str "single"])

      , testCase "multi word" $
        parseJira emph "_multiple words_" @?=
        Right (Emph [Str "multiple", Space, Str "words"])

      , testCase "require space before opening underscore" $
        isLeft (parseJira (str *> emph) "foo_bar_") @? "space after opening char"

      , testCase "disallow space after opening underscore" $
        isLeft (parseJira emph "_ nope_") @? "space after underscore"

      , testCase "require word boundary after closing underscore" $
        isLeft (parseJira emph "_nope_nope") @? "no boundary after closing"

      , testCase "zero with space as word boundary" $
        parseJira ((,) <$> emph <*> str) "_yup_\8203next" @?=
        Right (Emph [Str "yup"], Str "\8203next")

      , testCase "fails for strong" $
        isLeft (parseJira emph "*strong*") @? "strong as emph"
      ]

    , testCase "inserted" $
      parseJira inserted "+multiple words+" @?=
      Right (Inserted [Str "multiple", Space, Str "words"])

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

    , testGroup "strong"
      [ testCase "single word" $
        parseJira strong "*single*" @?= Right (Strong [Str "single"])

      , testCase "multi word" $
        parseJira strong "*multiple words*" @?=
        Right (Strong [Str "multiple", Space, Str "words"])

      , testCase "fails for emph" $
        isLeft (parseJira strong "_emph_") @? "emph as strong"
      ]

    , testCase "subscript" $
      parseJira subscript "~multiple words~" @?=
      Right (Subscript [Str "multiple", Space, Str "words"])

    , testCase "superscript" $
      parseJira superscript "^multiple words^" @?=
      Right (Superscript [Str "multiple", Space, Str "words"])

    , testGroup "link"
      [ testCase "unaliased link" $
        parseJira link "[https://example.org]" @?=
        Right (Link [] (URL "https://example.org"))

      , testCase "aliased link" $
        parseJira link "[Example|https://example.org]" @?=
        Right (Link [Str "Example"] (URL "https://example.org"))

      , testCase "alias with emphasis" $
        parseJira link "[_important_ example|https://example.org]" @?=
        Right (Link [Emph [Str "important"], Space, Str "example"]
                (URL "https://example.org"))
      ]

    , testGroup "image"
      [ testCase "local file" $
        parseJira image "!image.jpg!" @?=
        Right (Image (URL "image.jpg"))
      ]
    ]

  , testGroup "inline parser"
    [ testCase "simple sentence" $
      parseJira (many1 inline) "Hello, World!" @?=
      Right [Str "Hello,", Space, Str "World", Str "!"]
    ]
  ]
