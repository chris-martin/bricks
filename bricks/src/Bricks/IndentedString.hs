{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.IndentedString
  (
  -- * Indented string
    InStr
  , inStr'join
  , inStr'level
  , inStr'dedent
  , inStr'trim

  -- * Single line of an indented string
  , InStr'1 (..)
  , inStr'1'nonEmpty
  , inStr'1'empty
  , inStr'1'modifyLevel

  ) where

import Bricks.Expression

import Numeric.Natural (Natural)

import qualified Data.List as List
import qualified Data.Text as Text

{- | An "indented string literal," delimited by two single-quotes @''@.

This type of literal is called "indented" because the parser automatically
removes leading whitespace from the string ('inStr'dedent'), which makes it
convenient to use these literals for multi-line strings within an indented
expression without the whitespace from indentation ending up as part of the
string.
-}
type InStr = [InStr'1]

-- | One line of an 'InStr'.
data InStr'1 =
  InStr'1
    { inStr'1'level :: Natural
        -- ^ The number of leading space characters. We store this separately
        -- for easier implementation of 'inStr'dedent'.
    , inStr'1'str :: Str'Dynamic
        -- ^ The rest of the line after any leading spaces.
    }

-- | Join 'InStr's with newlines interspersed.
inStr'join :: InStr -> Str'Dynamic
inStr'join xs =
  List.concat $ List.intersperse [Str'1'Literal "\n"] (f <$> xs)
  where
    f :: InStr'1 -> [Str'1]
    f (InStr'1 n parts) =
      Str'1'Literal (Text.replicate (fromIntegral n) " ") : parts

{- | Determines whether an 'InStr'1' contains any non-space
characters. The opposite of 'inStr'1'nonEmpty'.

This is used to determine whether this line should be considered when
calculating the number of space characters to strip in 'inStr'dedent'. -}
inStr'1'nonEmpty :: InStr'1 -> Bool
inStr'1'nonEmpty =
  \case
    InStr'1{ inStr'1'str = [] } -> False
    _ -> True

-- | The opposite of 'inStr'1'nonEmpty'.
inStr'1'empty :: InStr'1 -> Bool
inStr'1'empty =
  not . inStr'1'nonEmpty

{- | Determine how many characters of whitespace to strip from an indented
string. -}
inStr'level :: InStr -> Natural
inStr'level xs =
  case List.filter inStr'1'nonEmpty xs of
    [] -> 0
    ys -> List.minimum (inStr'1'level <$> ys)

-- | Modify an 'InStr' by applying a function to its number of leading spaces.
inStr'1'modifyLevel :: (Natural -> Natural) -> (InStr'1 -> InStr'1)
inStr'1'modifyLevel f x@InStr'1{inStr'1'level = a} =
  x{ inStr'1'level = f a }

{- | Determine the minimum indentation of any nonempty line, and remove that
many space characters from the front of every line. -}
inStr'dedent :: InStr -> InStr
inStr'dedent xs =
  let
    b = inStr'level xs
    f a = if a >= b then a - b else 0
  in
    inStr'1'modifyLevel f <$> xs

-- | Remove any empty lines from the beginning or end of an indented string.
inStr'trim :: InStr -> InStr
inStr'trim xs =
  trimWhile inStr'1'empty xs
  where
    trimWhile f = List.dropWhileEnd f . List.dropWhile f
