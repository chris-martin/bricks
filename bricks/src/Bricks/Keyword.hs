{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Keyword
  (
  -- * Type
    Keyword

  -- * List of keywords
  , keywords

  -- * The keywords
  , keyword'rec
  , keyword'let
  , keyword'in
  , keyword'with
  , keyword'inherit
  , keyword'inlineComment

  -- * Type conversion
  , keywordString
  , keywordText

  ) where

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

newtype Keyword =
  Keyword
    { keywordText :: Text
    }

{- | All of the keywords. This list is used when parsing and rendering because
an unquoted string cannot have a name that is exactly the same as a keyword. -}
keywords :: [Keyword]
keywords =
  [ keyword'rec
  , keyword'let
  , keyword'in
  , keyword'with
  , keyword'inherit
  , keyword'inlineComment
  ]

keywordString :: Keyword -> String
keywordString = Text.unpack . keywordText

keyword'rec :: Keyword
keyword'rec = Keyword "rec"

keyword'let :: Keyword
keyword'let = Keyword "let"

keyword'in :: Keyword
keyword'in = Keyword "in"

keyword'with :: Keyword
keyword'with = Keyword "with"

keyword'inherit :: Keyword
keyword'inherit = Keyword "inherit"

keyword'inlineComment :: Keyword
keyword'inlineComment = Keyword "--"
