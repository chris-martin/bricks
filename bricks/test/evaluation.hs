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
import           Hedgehog (Property, property, (===))
import qualified Hedgehog

-- Base
import Prelude   (Int, Integer, (+))
import System.IO (IO)
import Text.Read (readMaybe)
import Text.Show (show)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_evaluation :: Property
prop_evaluation = property $ do

  let (Right expr) = P.parse parse'expression ""
        [text|{ add, int, ... }:
             |add (int "1") (int "2")|]

  result <- liftIO $ reduce'to'type'or'throw type'integer $
              expression'to'term expr /@\ standard'library

  result === 3
