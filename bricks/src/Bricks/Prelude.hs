{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bricks.Prelude
  ( bricks'parse
  , bricks'parse'file
  , bricks'parse'file'string'list
  , bricks'eval
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
import Bricks.Internal.Seq     (Seq)
import Bricks.Internal.Text    (Text)

-- parsec
import qualified Text.Parsec as P

-- path
import           Path (Path)
import qualified Path

-- path-text-utf8
import Path.Text.UTF8 (readFile)

-- base
import Control.Exception (Exception, throwIO)
import Data.Traversable  (for)
import Data.Typeable     (Typeable)
import System.IO         (IO)

data BricksError
  = BricksError'Parse P.ParseError
  | BricksError'Miscellaneous Text
  deriving Show

newtype BricksException =
  BricksException BricksError
  deriving Show

instance Exception BricksException

throw'parseError :: P.ParseError -> IO a
throw'parseError =
  throwIO . BricksException . BricksError'Parse

throw'miscellaneous :: Text -> IO a
throw'miscellaneous =
  throwIO . BricksException . BricksError'Miscellaneous

bricks'parse :: HasCallStack => Text -> IO Expression
bricks'parse =
  either throw'parseError pure . P.parse parse'file ""

bricks'parse'file :: HasCallStack => Path base Path.File -> IO Expression
bricks'parse'file path =
  readFile path >>= bricks'parse

bricks'parse'file'string'list :: HasCallStack
  => Path base Path.File -> IO (Seq Str'Static)
bricks'parse'file'string'list path =
  bricks'parse'file path >>=
  \case
    Expr'List list ->
      for (list'expressions list) $
        \case
          Expr'Str (str'dynamic'to'static -> Just s) -> pure s
          Expr'Str'Indented (inStr'to'strStatic -> Just s) -> pure s
          _ -> throw'miscellaneous "List item is not a static string"
    _ -> throw'miscellaneous "Expression is not a list"

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
    expr <- bricks'parse src
    expression'to'term expr
