{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module ChrisMartinOrg.CommandLineOpts
  ( Opts (..)
  , Command (..)
  , Verbosity (..)
  , getOpts
  ) where

import Paths_bricks (version)

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Version (showVersion)

import Options.Applicative

data Verbosity = VerbosityLow | VerbosityNormal | VerbosityHigh
  deriving Show

data Opts = Opts Verbosity Command
  deriving Show

data Command = Interpret Text
  deriving Show

interpretCommand :: Mod CommandFields Command
interpretCommand =
  command "interpret" $ info parser (progDesc "Parse a .nix file")
  where
    parser =
      Interpret <$>
      strArgument (metavar "NIX_EXPR" <> help "Nix expression to interpret")

getOpts :: IO Opts
getOpts =
    execParser $ info parser (progDesc "The chris-martin.org static site builder")
  where
    parser =
      helper <*>
      infoOption (showVersion version) (long "version" <> help "Show version") <*>
      (Opts <$> verbosityParser <*> hsubparser interpretCommand)
    verbosityParser =
      option readVerbosity (long "verbosity" <> help "Output level: low, normal, or high") <|>
      flag' VerbosityLow (short 'q' <> long "quiet" <> help "--verbosity low") <|>
      flag' VerbosityHigh (long "verbose" <> help "--verbosity high") <|>
      pure VerbosityNormal
    readVerbosity =
      maybeReader $ \case
        "low"    -> Just VerbosityLow
        "normal" -> Just VerbosityNormal
        "high"   -> Just VerbosityHigh
        _        -> Nothing
