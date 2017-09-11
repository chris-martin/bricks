{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Keywords

  ( keywords

  -- * Type conversion
  , keywordString
  , keywordText

  -- * The keywords
  , keyword'rec
  , keyword'let
  , keyword'in
  , keyword'with

  ) where

import Bricks.Types

import Data.String (String)
import Data.Text   (Text)

import qualified Data.Text as Text

{- | All of the keywords. This list is used when parsing and rendering because
a bare identifier cannot have a name that is exactly the same as a keyword. -}
keywords :: [Keyword]
keywords =
  [ keyword'rec
  , keyword'let
  , keyword'in
  , keyword'with
  ]

keywordString :: Keyword -> String
keywordString (Keyword x) = Text.unpack x

keywordText :: Keyword -> Text
keywordText (Keyword x) = x

keyword'rec :: Keyword
keyword'rec = Keyword "rec"

keyword'let :: Keyword
keyword'let = Keyword "let"

keyword'in :: Keyword
keyword'in = Keyword "in"

keyword'with :: Keyword
keyword'with = Keyword "with"
