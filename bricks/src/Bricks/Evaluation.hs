{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Bricks.Evaluation where

-- Bricks
import Bricks.Expression
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)

-- Containers
import Data.Map (Map)

-- Base
import Data.IORef
import System.IO  (IO)
