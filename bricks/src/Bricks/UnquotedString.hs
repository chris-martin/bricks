{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.UnquotedString
  (
  -- * Type
    Str'Unquoted (..)

  -- * Constructor
  , str'tryUnquoted

  -- * Predicates
  , str'canRenderUnquoted
  , char'canRenderUnquoted

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
conservative set of characters; see 'str'canRenderUnquoted' for the full rules.

The constructor is tagged "unsafe" because it lets you construct and invalid
value. Prefer 'bareMaybe' which does validate the text. -}
newtype Str'Unquoted = Str'Unquoted'Unsafe { str'unquotedToStatic :: Text }

str'tryUnquoted :: Text -> Maybe Str'Unquoted
str'tryUnquoted x =
  if str'canRenderUnquoted x then Just (Str'Unquoted'Unsafe x) else Nothing

{- | Whether a string having this name can be rendered without quoting it.
We allow a string to render unquoted if all these conditions are met:

- The string is nonempty
- All characters satify 'canBeBare'char'
- The string is not a keyword

>>> str'canRenderUnquoted "-ab_c"
True

>>> str'canRenderUnquoted ""
False

>>> str'canRenderUnquoted "a\"b"
False

>>> str'canRenderUnquoted "let"
False

-}
str'canRenderUnquoted :: Text -> Bool
str'canRenderUnquoted x =
  Text.all char'canRenderUnquoted x
  && not (Text.null x)
  && List.all ((/= x) . keywordText) keywords

-- | Letters, @-@, and @_@.
char'canRenderUnquoted :: Char -> Bool
char'canRenderUnquoted c =
  Char.isLetter c || c == '-' || c == '_'
