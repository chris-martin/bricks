{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

-- Bricks
import Bricks

-- Bricks internal
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
import Prelude   ((+), Int, Integer)
import System.IO (IO)
import Text.Show (show)
import Text.Read (readMaybe)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_evaluation :: Property
prop_evaluation = property $ do

  let
    expression'to'term = undefined
    dictTerm'fromList = undefined
    function'of'data = undefined
    function'of'str = undefined
    evaluate'as'data = undefined
    (/@\) = undefined

  Right f <- expression'to'term $ P.parse parse'expression
    [text|{ add, int, ... }:
         |add (int "1") (int "2")|]

  let args = dictTerm'fromList $
        [ ("add", function'of'data (+))
        , ("int", function'of'str $
            readMaybe @Integer >>>
            maybe (Left "invalid int") Right)
        ]

  result <- evaluate'as'data (f /@\ args)
  result === (3 :: Integer)
