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
--  Classes
--------------------------------------------------------------------------------

class JoinSourceInfo a
  where
    joinSourceInfo :: a -> a -> a

class FromSourceRange a
  where
    fromSourceRange :: SourceRange -> a

class ShowSourceInfo a
  where
    showSourceInfo :: a -> Maybe Text


--------------------------------------------------------------------------------
--  SourcePosition
--------------------------------------------------------------------------------

-- | An endpoint of a 'SourceRange'.
data SourcePosition =
  SourcePosition
    { sourcePosition'line :: Natural
    , sourcePosition'column :: Natural
    }
  deriving (Eq, Ord)

instance ShowSourceInfo SourcePosition
  where
    showSourceInfo (SourcePosition a b) =
      Just $ (Text.pack . show @Natural $ a) <> ":" <>
             (Text.pack . show @Natural $ b)


--------------------------------------------------------------------------------
--  SourceRange
--------------------------------------------------------------------------------

-- | Start and end points for a span of text. When we parse text into an
-- 'Expression', we annotate it with source ranges so error messages that
-- refer to specific expressions can tell the user where in their Bricks code
-- those expressions are defined.
data SourceRange =
  SourceRange
    { sourceRange'start :: SourcePosition
    , sourceRange'end :: SourcePosition
    }

instance JoinSourceInfo SourceRange
  where
    SourceRange a1 a2 `joinSourceInfo` SourceRange b1 b2 =
      SourceRange (min a1 b1) (max a2 b2)


--------------------------------------------------------------------------------
--  SourceName
--------------------------------------------------------------------------------

newtype SourceName a = SourceName a


--------------------------------------------------------------------------------
--  Unit instances
--------------------------------------------------------------------------------

instance JoinSourceInfo ()
  where
    joinSourceInfo _ _ = ()

instance FromSourceRange ()
  where
    fromSourceRange _ = ()

instance ShowSourceInfo ()
  where
    showSourceInfo _ = Nothing
