{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Bricks.IndentedString
  (
  -- * Indented string
    InStr (..)
  , inStr'to'strDynamic
  , inStr'level
  , inStr'dedent
  , inStr'trim
  , inStr'toList
  , inStr'discardSource

  -- * Single line of an indented string
  , InStr'1 (..)
  , inStr'1'toStrParts
  , inStr'1'discardSource

  ) where

-- Bricks
import Bricks.Expression
import Bricks.Source

-- Bricks internal
import qualified Bricks.Internal.List    as List
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

{- | An "indented string literal," delimited by two single-quotes @''@.

This type of literal is called "indented" because the parser automatically
removes leading whitespace from the string ('inStr'dedent'), which makes it
convenient to use these literals for multi-line strings within an indented
expression without the whitespace from indentation ending up as part of the
string. -}

data InStr =
  InStr
    { inStr'toSeq :: Seq InStr'1
    , inStr'source :: Maybe SourceRange
    }

inStr'toList :: InStr -> [InStr'1]
inStr'toList =
  Seq.toList . inStr'toSeq

inStr'discardSource :: InStr -> InStr
inStr'discardSource x =
  InStr
    { inStr'toSeq = fmap inStr'1'discardSource (inStr'toSeq x)
    , inStr'source = Nothing
    }

{- | One line of an 'InStr'. -}

data InStr'1 =
  InStr'1
    { inStr'1'level :: Natural
        -- ^ The number of leading space characters. We store this separately
        -- for easier implementation of 'inStr'dedent'.
    , inStr'1'indentSource :: Maybe SourceRange
        -- ^ The source position of the leading space characters
    , inStr'1'str :: Seq Str'1
        -- ^ The meat of the line, after any leading spaces and before the line
        --   break.
    , inStr'1'lineBreak :: Maybe Str'Static
        -- ^ The line break at the end, if any; all lines but the last one
        --   should have a line break
    }

inStr'1'discardSource :: InStr'1 -> InStr'1
inStr'1'discardSource x =
  InStr'1
    { inStr'1'level = inStr'1'level x
    , inStr'1'indentSource = Nothing
    , inStr'1'str = fmap str'1'discardSource (inStr'1'str x)
    , inStr'1'lineBreak = fmap str'static'discardSource (inStr'1'lineBreak x)
    }

inStr'1'toStrParts :: InStr'1 -> Seq Str'1
inStr'1'toStrParts x =
  indent <> inStr'1'str x <> end

  where
    indent :: Seq Str'1
    indent =
      case inStr'1'level x of
        0 -> Seq.empty
        level ->
          Seq.singleton . Str'1'Literal $
          Str'Static
            (Text.replicate (fromIntegral level) " ")
            (inStr'1'indentSource x)

    end :: Seq Str'1
    end =
      maybe Seq.empty (Seq.singleton . Str'1'Literal) $
      inStr'1'lineBreak x

{- | Determine how many characters of whitespace to strip from an indented
string. -}

inStr'level :: InStr -> Natural
inStr'level =
  maybe 0 id
  . List.minimum
  . catMaybes
  . List.map (\x ->
      if Seq.null (inStr'1'str x)
      then Nothing
      else Just (inStr'1'level x)
    )
  . inStr'toList

{- | Determine the minimum indentation of any nonempty line, and remove that
many space characters from the front of every line. -}

inStr'dedent :: InStr -> InStr
inStr'dedent x =
  let
    b = inStr'level x
  in
    x { inStr'toSeq = inStr'toSeq x <&>
        (\l ->
          l { inStr'1'level = let a = inStr'1'level l
                              in  if a >= b then a - b else 0
            })
      }

inStr'to'strDynamic :: InStr -> Str'Dynamic
inStr'to'strDynamic =
  inStr'trim >>>
  inStr'dedent >>>
  (\inStr ->
    Str'Dynamic
      (Seq.concatMap inStr'1'toStrParts (inStr'toSeq inStr))
      (inStr'source inStr)
  ) >>>
  str'dynamic'normalize

{- | Remove any empty lines from the beginning or end of an indented string,
and remove the newline from the final nonempty line. -}

inStr'trim :: InStr -> InStr
inStr'trim x =
  x { inStr'toSeq = inStr'toSeq x
        & Seq.trimWhile (Seq.null . inStr'1'str)
        & Seq.adjustLast (\y -> y { inStr'1'lineBreak = Nothing })
    }

show'inStr :: InStr -> Text
show'inStr x =
  "InStr [" <>
  Text.intercalateMap ", " show'inStr'1 (inStr'toSeq x) <> "]" <> " " <>
  Text.show (fmap show'sourceRange (inStr'source x))

show'inStr'1 :: InStr'1 -> Text
show'inStr'1 x =
  "InStr'1 " <>
  Text.show @Natural (inStr'1'level x) <> " " <>
  Text.show (fmap show'sourceRange (inStr'1'indentSource x)) <> " " <>
  Text.show @([Str'1]) (Seq.toList (inStr'1'str x)) <> " " <>
  Text.show @(Maybe Str'Static) (inStr'1'lineBreak x)

instance Show InStr   where show = Text.unpack . show'inStr
instance Show InStr'1 where show = Text.unpack . show'inStr'1
