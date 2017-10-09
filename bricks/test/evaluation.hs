{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

-- Bricks
import Bricks.Prelude
import Bricks.Type

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Hedgehog
import           Hedgehog (Property, property, withTests, (===))
import qualified Hedgehog

-- Base
import System.IO (IO)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_str :: Property
prop_str = withTests 1 $ property $ do

  do
    x <- liftIO $ bricks'eval type'string [text|"abc"|]
    x === "abc"

  do
    x <- liftIO $ bricks'eval type'string [text|''a${"b"}c''|]
    x === "abc"

{-
prop_dict :: Property
prop_dict = withTests 1 $ property $ do

  x <- liftIO $ bricks'eval type'string [text|{ a = "1"; }.a|]
  x === "1"

  x <- liftIO $ bricks'eval type'string [text|{ "a b" = "1"; }."a b"|]
  x === "1"
-}

prop_add :: Property
prop_add = withTests 1 $ property $ do

  do
    x <- liftIO $ bricks'eval'stdlib type'integer
      [text|┃{ add, integer, ... }:
            ┃add (integer "1") (integer "2")|]
    x === 3

  do
    x <- liftIO $ bricks'eval'stdlib type'integer
      [text|┃lib:
            ┃let
            ┃  inherit (lib) add integer;
            ┃in
            ┃  add (integer "1") (integer "2")|]
    x === 3
