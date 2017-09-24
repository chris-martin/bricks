{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Bricks.Type where

-- Bricks
import Bricks.Term

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Containers
import Data.Map (Map)
import Data.Set (Set)

-- Base
import           Data.Dynamic  (Dynamic, toDyn)
import           Data.IORef
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Typeable (Typeable)
import           Prelude       (Integer)
import           System.IO     (IO)

data Type a = Type { type'name :: Text }

type'boolean :: Type Bool
type'boolean = Type "boolean"

type'string :: Type Text
type'string = Type "string"

type'integer :: Type Integer
type'integer = Type "integer"

termTypeName :: MonadIO m => Term -> m Text
termTypeName = \case
  Term'Data x _   -> pure x
  Term'Function _ -> pure "built-in function"
  Term'Lambda _ _ -> pure "lambda"
  Term'LetRec _ _ -> pure "recursive let"
  Term'List _     -> pure "list"
  Term'Dict _     -> pure "dict"
  Term'Var _      -> pure "variable"
  Term'Apply _ _  -> pure "function application"
  Term'Pointer p  -> readTermPtr p >>= termTypeName
