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

import qualified Data.Text as Text

tests :: TestTree
tests = testGroup "Blocks"
  [ testGroup "components"
    [ testGroup "para"
      [ testCase "two lines" $
        parseJira block "one\ntwo\n" @?=
        Right (Para [Str "one", Linebreak, Str "two"])

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

    , testGroup "header"
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

    , testGroup "list"
      [ testCase "single item list" $
        parseJira list "* hello\n" @?=
        Right (List CircleBullets [[Para [Str "hello"]]])

      , testCase "simple list" $
        let text = Text.unlines
                   [ "* one"
                   , "* two"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [Para [Str "one"]]
                  , [Para [Str "two"]]])

      , testCase "list followed by different list" $
        parseJira ((,) <$> list <*> list) "- first\n* second\n" @?=
        Right ( List SquareBullets [[Para [Str "first"]]]
              , List CircleBullets [[Para [Str "second"]]])

      , testCase "nested lists" $
        parseJira list "* first\n** nested\n" @?=
        Right (List CircleBullets
               [
                 [ Para [Str "first"]
                 , List CircleBullets [[Para [Str "nested"]]]
                 ]
               ])

      , testCase "deeply nested list" $
        parseJira list "*-* nested\n*-* list\n" @?=
        Right (List CircleBullets
               [
                 [ List SquareBullets
                   [[ List CircleBullets
                      [ [Para [Str "nested"]]
                      , [Para [Str "list"]]]
                    ]]
                 ]
               ])

      , testCase "markers can vary" $
        parseJira list "#-* nested\n*** list\n" @?=
        Right (List Enumeration
               [[ List SquareBullets
                  [[ List CircleBullets
                     [ [Para [Str "nested"]]
                     , [Para [Str "list"]]
                     ]
                   ]]
                ]])

      , testCase "single nested list after paragraph" $
        let text = Text.unlines
                   [ "* line"
                   , "continued"
                   , "** nested"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "line", Linebreak, Str "continued"]
                    , List CircleBullets [[Para [Str "nested"]]]]])

      , testCase "multiple nested lists after paragraph" $
        let text = Text.unlines
                   [ "* line"
                   , "continued"
                   , "** nested"
                   , "*# another"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "line", Linebreak, Str "continued"]
                    , List CircleBullets [[Para [Str "nested"]]]
                    , List Enumeration [[Para [Str "another"]]]]])

      , testCase "item after nested list" $
        let text = Text.unlines
                   [ "* first"
                   , "** nested"
                   , "* second"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "first"]
                    , List CircleBullets [[Para [Str "nested"]]]]
                  , [ Para [Str "second"]]])

      , testCase "nested lists" $
        let text = Text.unlines
                   [ "** eins"
                   , "*- zwei"
                   , "** drei"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ List CircleBullets [[Para [Str "eins"]]]
                    , List SquareBullets [[Para [Str "zwei"]]]
                    , List CircleBullets [[Para [Str "drei"]]]
                    ]
                  ])
      ]
    ]

  , testGroup "block combinations"
    [ testCase "single paragraph" $
      parseJira block "Lorem ipsum." @?=
      Right (Para [Str "Lorem", Space, Str "ipsum."])

    , testCase "para before header" $
      parseJira ((,) <$> block <*> block) "paragraph\nh1.header\n" @?=
      Right (Para [Str "paragraph"], Header 1 [Str "header"])

    , testCase "para after header" $
      parseJira ((,) <$> block <*> block) "h2.header\nparagraph\n" @?=
      Right (Header 2 [Str "header"], Para [Str "paragraph"])

    , testCase "para after list" $
      parseJira ((,) <$> block <*> block) "* foo\n\nbar\n" @?=
      Right (List CircleBullets [[Para [Str "foo"]]], Para [Str "bar"])

    , testCase "successive lists of same type" $
      parseJira ((,) <$> block <*> block) "* foo\n\n* bar\n" @?=
      Right ( List CircleBullets [[Para [Str "foo"]]]
            , List CircleBullets [[Para [Str "bar"]]])
    ]
  ]
