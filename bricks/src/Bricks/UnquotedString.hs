{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.UnquotedString
  (
  -- * Type
    Str'Unquoted (..)

  -- * Constructor
  , bareMaybe

  -- * Predicates
  , canBeBare'str
  , canBeBare'char

  ) where

-- Bricks
import Bricks.Keyword

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Base
import qualified Data.Char as Char
import qualified Data.List as List

{- | A string that can be rendered unquoted. Bare strings are restricted to a
conservative set of characters; see 'canBeBare'str' for the full rules.

The constructor is tagged "unsafe" because it lets you construct and invalid
value. Prefer 'bareMaybe' which does validate the text. -}
newtype Str'Unquoted = BareUnsafe { bare'str :: Text }

bareMaybe :: Text -> Maybe Str'Unquoted
bareMaybe x =
  if canBeBare'str x then Just (BareUnsafe x) else Nothing

{- | Whether a string having this name can be rendered without quoting it.
We allow a string to render unquoted if all these conditions are met:

- The string is nonempty
- All characters satify 'canBeBare'char'
- The string is not a keyword

>>> canBeBare'str "-ab_c"
True

>>> canBeBare'str ""
False

>>> canBeBare'str "a\"b"
False

>>> canBeBare'str "let"
False

-}
canBeBare'str :: Text -> Bool
canBeBare'str x =
  Text.all canBeBare'char x
  && not (Text.null x)
  && List.all ((/= x) . keywordText) keywords

-- | Letters, @-@, and @_@.
canBeBare'char :: Char -> Bool
canBeBare'char c =
  Char.isLetter c || c == '-' || c == '_'
