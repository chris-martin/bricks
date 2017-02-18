module ChrisMartinOrg.Css
    ( styleLink
    , compileCss
    , compileCssSource
    , compileCssFallback
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Prelude

import ChrisMartinOrg.Hash (writeHashBS)

import Data.Default
import Data.Semigroup

import qualified System.Directory            as Dir
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Sass                   as Sass
import qualified Text.Sass.Compilation       as SassC

import Text.Blaze.Html5 as H hiding (main)
import Text.Sass.Options (SassOptions (..), SassOutputStyle (..))

compileCss :: Css -> IO (Either String CompiledCss)
compileCss (CssSource path) = compileCssSource path
compileCss (CssCompiled path) = pure $ Right path

compileCssFallback :: [Css] -> IO (Maybe CompiledCss)
compileCssFallback [] = pure Nothing
compileCssFallback (x:xs) = do
    e <- compileCss x
    either fail (return . Just) e
  where
    fail err = do putStrLn err
                  compileCssFallback xs

compileCssSource :: FilePath -> IO (Either String CompiledCss)
compileCssSource inFile = do
    exists <- Dir.doesFileExist inFile
    if exists
      then do
        result <- Sass.compileFile inFile sassOpts
        case result of
            Left err -> Left <$> SassC.errorMessage err
            Right bs -> Right <$> CompiledCss <$> writeHashBS bs "css"
      else do
        return $ Left ("Missing CSS: " <> inFile)

sassOpts :: SassOptions
sassOpts = def { sassOutputStyle = SassStyleCompact }

styleLink :: CompiledCss -> Html
styleLink href = link
    ! A.rel "stylesheet"
    ! A.type_ "text/css"
    ! A.href (fromString $ compiledCssPath href)
