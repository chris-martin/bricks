{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Css
    ( styleLink
    , compileCss
    , compileCssSource
    , compileCssFallback
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Hash (writeHashBS)

import Data.Default
import Data.String  (IsString (..))

import qualified System.Directory as Dir

import           Text.Blaze.Html5            as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Sass             as Sass
import qualified Text.Sass.Compilation as SassC
import           Text.Sass.Options     (SassOptions (..), SassOutputStyle (..))

compileCss :: Css -> IO (Maybe CompiledCss)
compileCss (CssSource path) = compileCssSource path
compileCss (CssCompiled path) = pure $ Just path

compileCssFallback :: Fallback Css -> IO (Maybe CompiledCss)
compileCssFallback xs = firstJust <$> sequence (compileCss <$> xs)

compileCssSource :: FilePath -> IO (Maybe CompiledCss)
compileCssSource inFile = do
    exists <- Dir.doesFileExist inFile
    if exists
      then do
        result <- Sass.compileFile inFile sassOpts
        case result of
            Left err -> do
                SassC.errorMessage err >>= putStrLn
                return Nothing
            Right bs -> (Just . CompiledCss) <$> writeHashBS bs "css"
      else do
        _ <- putStrLn $ "Missing CSS: " ++ inFile
        return Nothing

sassOpts :: SassOptions
sassOpts = def { sassOutputStyle = SassStyleCompact }

styleLink :: CompiledCss -> Html
styleLink href = link
    ! A.rel "stylesheet"
    ! A.type_ "text/css"
    ! A.href (fromString $ compiledCssPath href)
