{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{- |

Source information is some form of description of where a Bricks expression or
term /came from/. This information is used to tag 'Expression's and 'Term's so
that our error messages can tell the user what parts of the Bricks source code
are relevant to the problem.

Overview of the types involved:

  - 'SourcePosition' - a line and column number
  - 'SourceRange' - a pair of 'SourcePosition's indicating the start and end of
    some expression
  - 'SourceName' - a description of where the top-level expression was parsed
    from, such as a file path or a part of a REPL session.
  - '()' can be used as the source information type in cases where we do not
    actually care to retain any source information.

-}
module Bricks.Source where

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text


--------------------------------------------------------------------------------
--  SourcePosition
--------------------------------------------------------------------------------

{- | An endpoint of a 'SourceRange'. -}

data SourcePosition =
  SourcePosition
    { sourcePosition'line :: Natural
    , sourcePosition'column :: Natural
    }
  deriving (Eq, Ord)

show'sourcePosition :: SourcePosition -> Text
show'sourcePosition (SourcePosition a b) =
  Text.show @Natural a <> ":" <> Text.show @Natural b


--------------------------------------------------------------------------------
--  SourceRange
--------------------------------------------------------------------------------

{- | Start and end points for a span of text. When we parse text into an
'Expression', we annotate it with source ranges so error messages that refer to
specific expressions can tell the user where in their Bricks code those
expressions are defined. -}

data SourceRange =
  SourceRange
    { sourceRange'start :: SourcePosition
    , sourceRange'end :: SourcePosition
    }

sourceRange'join :: SourceRange -> SourceRange -> SourceRange
sourceRange'join (SourceRange a1 a2) (SourceRange b1 b2) =
  SourceRange (min a1 b1) (max a2 b2)

instance Semigroup SourceRange
  where
    (<>) = sourceRange'join


--------------------------------------------------------------------------------
--  SourceRangeMaybe
--------------------------------------------------------------------------------

data SourceRangeMaybe
  = SourceRange'Nothing
  | SourceRange'Just SourceRange

sourceRangeMaybe'join
  :: SourceRangeMaybe -> SourceRangeMaybe -> SourceRangeMaybe
sourceRangeMaybe'join (SourceRange'Just x) (SourceRange'Just y) =
  SourceRange'Just (sourceRange'join x y)
sourceRangeMaybe'join _ _ = SourceRange'Nothing

instance Semigroup SourceRangeMaybe
  where
    (<>) = sourceRangeMaybe'join


--------------------------------------------------------------------------------
--  SourceName
--------------------------------------------------------------------------------

newtype SourceName a = SourceName a
