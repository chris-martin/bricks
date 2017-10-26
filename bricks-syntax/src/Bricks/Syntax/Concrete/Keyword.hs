{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Syntax.Concrete.Keyword
  (
  -- * Type
    Keyword

  -- * List of keywords
  , keywords

  -- * The keywords
  , keyword'rec, keyword'let, keyword'in, keyword'inherit, keyword'inlineComment

  -- * Type conversion
  , keyword'text

  ) where

-- Bricks internal
import Bricks.Internal.Prelude
import Bricks.Internal.Text    (Text)

newtype Keyword = Keyword Text
  deriving Show

keyword'text :: Keyword -> Text
keyword'text (Keyword x) = x

{- | All of the keywords. This list is used when parsing and rendering because
an unquoted string cannot have a name that is exactly the same as a keyword. -}

keywords :: [Keyword]
keywords =
  [ keyword'rec
  , keyword'let
  , keyword'in
  , keyword'inherit
  , keyword'inlineComment
  ]

keyword'rec :: Keyword
keyword'rec = Keyword "rec"

keyword'let :: Keyword
keyword'let = Keyword "let"

keyword'in :: Keyword
keyword'in = Keyword "in"

keyword'inherit :: Keyword
keyword'inherit = Keyword "inherit"

keyword'inlineComment :: Keyword
keyword'inlineComment = Keyword "--"
