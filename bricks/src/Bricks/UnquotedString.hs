{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.UnquotedString
  (
  -- * Type
    UnquotedString

  -- * Construction
  , unquotedString'try
  , unquotedString'orThrow

  -- * Deconstruction
  , unquotedString'text

  -- * Predicates
  , text'canBeUnquoted
  , char'canBeUnquoted

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
a conservative set of characters; see 'text'canBeUnquoted' for the full rules.

This type does not represent a particular part of Bricks syntax, but it is a
wrapper for 'Text' that enforces the limitations of strings at various places in
the Bricks syntax.

==== Construction

  - 'unquotedString'try'
  - 'unquotedString'orThrow'

==== Deconstruction

  - 'unquotedString'text'

==== See also

  - 'text'canBeUnquoted'
  - 'char'canBeUnquoted'

-}

newtype UnquotedString = UnquotedString { unquotedString'text :: Text }

instance Show UnquotedString
  where
    showsPrec _ x = ("unquoted " <>) . shows (unquotedString'text x)

{- | ==== Properties

  - A text value may be used to construct an 'UnquotedString' iff it satisfies
    'text'canBeUnquoted'.

      @'text'canBeUnquoted' x = 'isJust' ('unquotedString'try' x)@ -}

unquotedString'try :: Text -> Maybe UnquotedString
unquotedString'try x =
  if text'canBeUnquoted x then Just (UnquotedString x) else Nothing

-- | Throws an exception if the string cannot render unquoted.
unquotedString'orThrow :: Text -> UnquotedString
unquotedString'orThrow x =
  if text'canBeUnquoted x then UnquotedString x else
  error $ "String " <> show x <> " cannot render unquoted"

{- | Whether a string having this name can be rendered without quoting it.

==== Requirements for unquoted strings

We allow a string to render unquoted if all these conditions are met:

- The string is nonempty
- All characters satify 'char'canBeUnquoted'
- The string is not a keyword

==== Properties

  - A text value may be used to construct an 'UnquotedString' iff it satisfies
    'text'canBeUnquoted'.

      @'text'canBeUnquoted' x = 'isJust' ('unquotedString'try' x)@ -}

-- | ==== Examples
--
-- >>> text'canBeUnquoted "-ab_c"
-- True
--
-- >>> text'canBeUnquoted ""
-- False
--
-- >>> text'canBeUnquoted "a\"b"
-- False
--
-- >>> text'canBeUnquoted "let"
-- False

text'canBeUnquoted :: Text -> Bool
text'canBeUnquoted x =
  Text.all char'canBeUnquoted x
  && not (Text.null x)
  && List.all ((/= x) . keywordText) keywords

{- | Whether the character is allowed to be included in an 'UnquotedString'.
Such characters are letters, @+@, @-@, @*@, @/@, and @_@.

This is used in the implementation of 'text'canBeUnquoted'. -}

char'canBeUnquoted :: Char -> Bool
char'canBeUnquoted c =
  Char.isLetter c || List.elem c ("+-*/_" :: [Char])
