module Bricks.Prelude
  ( bricks'eval'stdlib
  ) where

-- Bricks
import Bricks
import Bricks.Evaluation
import Bricks.ExpressionToTerm
import Bricks.Type
import Bricks.BuiltinFunctions
import Bricks.Term

-- Bricks internal
import Bricks.Internal.Text (Text)
import Bricks.Internal.Prelude

-- Parsec
import qualified Text.Parsec      as P

-- Base
import Data.Typeable (Typeable)

bricks'eval'stdlib :: (HasCallStack, Typeable a) => Type a -> Text -> IO a
bricks'eval'stdlib typ src =
  do
    expr <- either (error . show) pure (P.parse parse'expression "" src)
    term <- expression'to'term expr
    reduce'to'type'or'throw typ (term /@\ standard'library)
