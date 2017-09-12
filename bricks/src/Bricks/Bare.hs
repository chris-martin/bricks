{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Bare
  (
  -- * Type
    Bare (..)

  -- * Constructor
  , bareMaybe

  -- * Predicates
  , canBeBare'str
  , canBeBare'char

  ) where

import Bricks.Keyword

import Data.Bool     (Bool, not, (&&), (||))
import Data.Char     (Char)
import Data.Eq       ((/=), (==))
import Data.Function ((.))
import Data.Text     (Text)
import Data.Maybe (Maybe (..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text

{- | A string that can be rendered unquoted. Bare strings are restricted to a
conservative set of characters; see 'canBeBare'str' for the full rules.

The constructor is tagged "unsafe" because it lets you construct and invalid
value. Prefer 'bareMaybe' which does validate the text. -}
newtype Bare = BareUnsafe { bare'str :: Text }

bareMaybe :: Text -> Maybe Bare
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
