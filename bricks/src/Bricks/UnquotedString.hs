{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.UnquotedString
  (
  -- * Type
    UnquotedString (..)

  -- * Constructor
  , str'tryUnquoted
  , str'unquoted'orThrow

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
import           Prelude   (error)

{- | A string that can be rendered unquoted. Unquoted strings are restricted to
a conservative set of characters; see 'str'canRenderUnquoted' for the full
rules.

The constructor is tagged "unsafe" because it lets you construct and invalid
value. Prefer 'str'tryUnquoted' which does validate the text.

This type does not represent a particular part of Brick syntax, but it is a
wrapper for 'Text' that enforces the limitations of strings at various places
in the Bricks syntax. -}
newtype UnquotedString = UnquotedString'Unsafe { unquotedString'text :: Text }

instance Show UnquotedString
  where
    showsPrec _ x = ("unquoted " <>) . shows (unquotedString'text x)

str'tryUnquoted :: Text -> Maybe UnquotedString
str'tryUnquoted x =
  if str'canRenderUnquoted x then Just (UnquotedString'Unsafe x) else Nothing

-- | Throws an exception if the string cannot render unquoted.
str'unquoted'orThrow :: Text -> UnquotedString
str'unquoted'orThrow x =
  if str'canRenderUnquoted x then UnquotedString'Unsafe x else
  error $ "String " <> show x <> " cannot render unquoted"

{- | Whether a string having this name can be rendered without quoting it.
We allow a string to render unquoted if all these conditions are met:

- The string is nonempty
- All characters satify 'char'canRenderUnquoted'
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
