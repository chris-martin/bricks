{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-

This test suite verifies that the 'Show' instance for 'Expression' outputs code
which can be used to reconstruct the 'Expression' using the functions in
"Bricks.Expression.Construction".

For each of the example Haskell expressions in the @show-example.txt@ file, we
do the following:

  1. Use GHC to evaluate the Haskell expression to get a Bricks 'Expression'.
  2. Call 'show' on the 'Expression', and verify that the result is equal to the
     original input (ignoring any differences in whitespace).

-}

import Paths_bricks_syntax (getDataFileName)

-- Bricks
import Bricks.Expression

-- Bricks internal
import qualified Bricks.Internal.List    as List
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- text
import qualified Data.Text    as Text (lines)
import qualified Data.Text
import qualified Data.Text.IO as Text (hPutStr, readFile)

-- exceptions
import Control.Monad.Catch (try)

-- hint
import qualified Language.Haskell.Interpreter as H

-- base
import           Control.Monad (unless)
import qualified Data.Char     as Char
import           Data.Function (on)
import           Prelude       (Int, Num (..))
import qualified System.Exit   as Exit
import           System.IO     (IO)
import qualified System.IO     as IO

put :: Text -> IO ()
put = Text.hPutStr IO.stderr

main :: IO ()
main =
  do
    examples <- getExamples

    H.runInterpreter (interpreter'main examples) >>= \case

      Left err -> do
        put $ showInterpreterError err
        put "\n"
        Exit.exitFailure

      Right failures -> do
        traverse_ printFailure failures
        traverse_ put
          [ "Out of "
          , Text.show @Int $ List.length examples
          , " total tests run, there were "
          , Text.show @Int $ List.length examples - List.length failures
          , " success"
          , if List.length failures == 1 then "" else "es"
          , " and "
          , Text.show @Int $ List.length failures
          , " failure"
          , if List.length failures == 1 then "" else "s"
          , ".\n"
          ]

        unless (List.null failures) Exit.exitFailure

getExamples :: IO [Example]
getExamples =
  do
    path <- getDataFileName "../test-data/expression-show-examples.txt"
    text <- Text.readFile path
    pure (parseExamples text)

data Example =
  Example
    { example'lineNumber :: Natural
    , example'text :: Text
    }

parseExamples :: Text -> [Example]
parseExamples =
  List.map (\xs -> Example
    { example'lineNumber = fst (List.head xs)
    , example'text = Text.intercalateMap "\n" snd xs
    }) .
  catMaybes .
  List.map (either (const Nothing) Just) .
  List.groupEither .
  List.map (\(a, b) -> if b == "" then Left () else Right (a, b)) .
  List.zip [1..] .
  Text.lines

data Failure =
  Failure
    { failure'example :: Example
    , failure'outcome :: Text
    }

printFailure :: Failure -> IO  ()
printFailure Failure{ failure'example = Example { example'lineNumber }
                   , failure'outcome } =
  traverse_ put
    [ "For the example starting on line "
    , Text.show @Natural example'lineNumber
    , ", we got the following incorrect result: \n"
    , failure'outcome
    , "\n"
    , Text.replicate 40 "-"
    , "\n"
    ]

interpreter'main :: [Example] -> H.Interpreter [Failure]
interpreter'main examples =
  do
    H.set [ H.languageExtensions H.:= [ H.OverloadedStrings ] ]
    H.setImports
      [ "Bricks.Expression"
      , "Bricks.Expression.Construction"
      ]

    catMaybes <$> traverse testExample examples

testExample :: Example -> H.Interpreter (Maybe Failure)
testExample x =
  do
    e <- try $ H.interpret (Text.unpack $ example'text x) (H.as :: Expression)
    pure $ case e of
      Left err -> Just $ Failure
        { failure'example = x
        , failure'outcome = showInterpreterError err
        }
      Right expr ->
        let
          s = Text.show expr
        in
          if s ~= example'text x
            then Nothing
            else Just $ Failure
              { failure'example = x
              , failure'outcome = s
              }

-- | Equality, ignoring whitespace.
(~=) :: Text -> Text -> Bool
(~=) =
  (==) `on` f
  where
    f :: Text -> Text
    f = Data.Text.concatMap $ \c ->
          if Char.isSpace c then "" else Text.singleton c

showInterpreterError :: H.InterpreterError -> Text
showInterpreterError =
  \case
    H.WontCompile es ->
      Text.intercalateMap "\n" (\(H.GhcError e) -> Text.pack e) es
    e -> Text.show e
