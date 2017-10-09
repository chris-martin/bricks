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
import Bricks.Internal.Monad
import Bricks.Internal.Prelude
import Bricks.Internal.Text    (Text)

-- Base
import Prelude (Integer)

data Type a = Type { type'name :: Text }

type'boolean :: Type Bool
type'boolean = Type "boolean"

type'string :: Type Text
type'string = Type "string"

type'integer :: Type Integer
type'integer = Type "integer"

termTypeName :: MonadIO m => Term -> m Text
termTypeName = \case
  Term'Data x _ -> pure x
  Term'Function{} -> pure "built-in function"
  Term'Lambda{} -> pure "lambda"
  Term'LetRec{} -> pure "recursive let"
  Term'List{} -> pure "list"
  Term'Dict{} -> pure "dict"
  Term'Dict'ReducedKeys{} -> pure "dict with reduced keys"
  Term'Var{} -> pure "variable"
  Term'Apply{} -> pure "function application"
  Term'Pointer p -> readTermPtr p >>= termTypeName
