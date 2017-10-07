{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

-- Bricks
import Bricks.BuiltinFunctions
import Bricks.Evaluation
import Bricks.ExpressionToTerm
import Bricks.Parsing
import Bricks.Term
import Bricks.Type
import Bricks.Prelude

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Parsec
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

-- Hedgehog
import           Hedgehog (Property, property, (===), withTests)
import qualified Hedgehog

-- Base
import Prelude   (Int, Integer, (+))
import System.IO (IO)
import Text.Read (readMaybe)
import Text.Show (show)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_str :: Property
prop_str = withTests 1 $ property $ do

  x <- liftIO $ bricks'eval type'string [text|"abc"|]
  x === "abc"

  x <- liftIO $ bricks'eval type'string [text|''a${"b"}c''|]
  x === "abc"

prop_add :: Property
prop_add = withTests 1 $ property $ do

  x <- liftIO $ bricks'eval'stdlib type'integer
    [text|{ add, integer, ... }:
         |add (integer "1") (integer "2")|]
  x === 3

  x <- liftIO $ bricks'eval'stdlib type'integer
    [text|lib:
         |let
         |  inherit (lib) add integer;
         |in
         |  add (integer "1") (integer "2")|]
  x === 3
