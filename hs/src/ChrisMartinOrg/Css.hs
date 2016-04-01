{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Css
    ( styleLink
    , compileCss
    ) where

import ChrisMartinOrg.Core

import qualified Data.ByteString as BS
import           Data.Default
import           Data.String     (IsString (..))

import qualified System.Directory as Dir

import           Text.Blaze.Html5            as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Sass             as Sass
import qualified Text.Sass.Compilation as SassC
import           Text.Sass.Options     (SassOptions (..), SassOutputStyle (..))

compileCss :: FilePath -> IO (Maybe FilePath)
compileCss inFile = do
    exists <- Dir.doesFileExist inFile
    if exists
      then do
        result <- Sass.compileFile inFile sassOpts
        case result of
            Left err -> do
                SassC.errorMessage err >>= putStrLn
                return Nothing
            Right bs -> do
                let outFile = "css/" ++ (hash bs)
                BS.writeFile ("out/" ++ outFile) bs
                return $ Just outFile
      else do
        _ <- putStrLn $ "Missing CSS: " ++ inFile
        return Nothing

sassOpts :: SassOptions
sassOpts = def { sassOutputStyle = SassStyleCompact }

styleLink :: FilePath -> Html
styleLink href = link
    ! A.rel "stylesheet"
    ! A.type_ "text/css"
    ! A.href (fromString href)
