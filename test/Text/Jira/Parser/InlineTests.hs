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

      , testCase "non-special symbols" $
        parseJira str ",.;#%" @?= Right (Str ",.;#%")

      , testCase "umlauts" $
        parseJira str "äéíöüßðå" @?= Right (Str "äéíöüßðå")

      , testCase "space fails" $
        isLeft (parseJira str " ") @?
        "str should only be parsed into Space"
      ]

    , testGroup "symbol"
      [ testCase "special symbol" $
        parseJira symbol "!" @?= Right (SpecialChar '!')

      , testCase "escaped symbol" $
        parseJira symbol "\\{" @?= Right (SpecialChar '{')
      ]

    , testGroup "emoji"
      [ testCase "smiling face" $
        parseJira emoji ":D" @?= Right (Emoji IconSmiling)

      , testCase "winking face" $
        parseJira emoji ";)" @?= Right (Emoji IconWinking)

      , testCase "checkmark" $
        parseJira emoji "(/)" @?= Right (Emoji IconCheckmark)

      , testCase "red x" $
        parseJira emoji "(x)" @?= Right (Emoji IconX)

      , testCase "thumbs up" $
        parseJira emoji "(y)" @?= Right (Emoji IconThumbsUp)

      , testCase "green star" $
        parseJira emoji "(*g)" @?= Right (Emoji IconStarGreen)

      , testCase "may not be followed by a letter" $
        isLeft (parseJira emoji "(x)nope") @? "no letters after emojis"
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

    , testGroup "entity"
      [ testCase "named entity" $
        parseJira entity "&copy;" @?= Right (Entity "copy")

      , testCase "numerical entity" $
        parseJira entity "&#65;" @?= Right (Entity "#65")

      , testCase "invalid entity" $
        parseJira entity "&haskell;" @?= Right (Entity "haskell")

      , testCase "space" $
        isLeft (parseJira entity "&a b;") @?
        "entities may not contain spaces"

      , testCase "symbol" $
        isLeft (parseJira entity "&a-b;") @?
        "entities may not contain symbols"

      , testCase "number without hash" $
        isLeft (parseJira entity "&65;") @?
        "numerical entities must start with &#"

      , testCase "no name" $
        isLeft (parseJira entity "&;") @?
        "entities must not be empty"
      ]

    , testCase "deleted" $
      parseJira deleted "-far-fetched-" @?=
      Right (Deleted [Str "far", SpecialChar '-', Str "fetched"])

    , testGroup "emph"
      [ testCase "single word" $
        parseJira emph "_single_" @?= Right (Emph [Str "single"])

      , testCase "multi word" $
        parseJira emph "_multiple words_" @?=
        Right (Emph [Str "multiple", Space, Str "words"])

      , testCase "symbol before opening underscore" $
        parseJira (str *> emph) "#_bar_" @?=
        Right (Emph [Str "bar"])

      , testCase "neither symbol nor space before opening underscore" $
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

    , testCase "monospaced" $
      parseJira monospaced "{{multiple words}}" @?=
      Right (Monospaced [Str "multiple", Space, Str "words"])

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

    , testCase "anchor" $
      parseJira anchor "{anchor:testing}" @?=
      Right (Anchor "testing")

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

      , testCase "no newlines" $
        isLeft (parseJira image "!hello\nworld.png!") @?
        "no newlines in image names"
      ]
    ]

  , testGroup "inline parser"
    [ testCase "simple sentence" $
      parseJira (normalizeInlines <$> many1 inline) "Hello, World!" @?=
      Right [Str "Hello,", Space, Str "World", SpecialChar '!']

    , testCase "with entity" $
      parseJira (many1 inline) "shopping at P&amp;C" @?=
      Right [ Str "shopping", Space, Str "at", Space
            , Str "P", Entity "amp", Str "C"
            ]

    , testCase "backslash-escaped char" $
      parseJira (normalizeInlines <$> many1 inline) "opening brace: \\{" @?=
      Right [Str "opening", Space, Str "brace:", Space, SpecialChar '{']

    , testCase "icon after word" $
      parseJira (many1 inline) "checkmark(/)" @?=
      Right [Str "checkmark", Emoji IconCheckmark]

    , testCase "smiley after word" $
      parseJira (normalizeInlines <$> many1 inline) "smiley:)" @?=
      Right [Str "smiley", Emoji IconSlightlySmiling]

    , testCase "escaped smiley after word" $
      parseJira (normalizeInlines <$> many1 inline) "closing paren\\:)" @?=
      Right [Str "closing", Space, Str "paren", SpecialChar ':', Str ")"]

    , testCase "smiley between words" $
      parseJira (normalizeInlines <$> many1 inline) "verdict: :D funny" @?=
      Right [Str "verdict:", Space, Emoji IconSmiling, Space, Str "funny"]
    ]
  ]
