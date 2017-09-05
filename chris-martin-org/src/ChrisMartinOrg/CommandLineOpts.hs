{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.CommandLineOpts
  ( Opts (..)
  , Command (..)
  , getOpts
  ) where

import Paths_chris_martin_org (version)

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Version (showVersion)

import Options.Applicative (Mod, CommandFields, strArgument, metavar, help, hsubparser, helper, execParser, info, progDesc, long, infoOption, command)

data Opts = Opts Command
  deriving Show

data Command = Interpret Text
  deriving Show

interpretCommand :: Mod CommandFields Command
interpretCommand =
  command "interpret" $ info parser (progDesc "Parse a .nix file")
  where
    parser =
      Interpret <$>
      strArgument (metavar "FILE" <> help "Path of the .nix file to interpret")

getOpts :: IO Opts
getOpts =
    execParser $ info parser (progDesc "The chris-martin.org static site builder")
  where
    parser =
      helper <*>
      infoOption (showVersion version) (long "version" <> help "Show version") <*>
      (Opts <$> hsubparser interpretCommand)
