{-# LANGUAGE FlexibleContexts #-}

module Bricks.Prelude
  ( bricks'eval
  , bricks'eval'stdlib
  ) where

-- Bricks
import Bricks
import Bricks.BuiltinFunctions
import Bricks.Evaluation
import Bricks.ExpressionToTerm
import Bricks.Term
import Bricks.Type

-- Bricks internal
import Bricks.Internal.Prelude
import Bricks.Internal.Text    (Text)

-- Parsec
import qualified Text.Parsec as P

-- Base
import Data.Typeable (Typeable)

bricks'eval :: (HasCallStack, Typeable a) => Type a -> Text -> IO a
bricks'eval typ src =
  do
    term <- source'to'term src
    reduce'to'type'or'throw typ term

bricks'eval'stdlib :: (HasCallStack, Typeable a) => Type a -> Text -> IO a
bricks'eval'stdlib typ src =
  do
    term <- source'to'term src
    reduce'to'type'or'throw typ (term /@\ standard'library)

source'to'term :: HasCallStack => Text -> IO Term
source'to'term src =
  do
    expr <- either (error . show) pure (P.parse parse'expression "" src)
    expression'to'term expr
