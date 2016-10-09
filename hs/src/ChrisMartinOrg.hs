{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg (main) where

import           ChrisMartinOrg.Core
import           ChrisMartinOrg.Css
import           ChrisMartinOrg.Hash (writeHashFile)
import qualified ChrisMartinOrg.Home as Home
import           ChrisMartinOrg.Post (getPosts, writePost)
import           Control.Monad       (when)

import Control.Monad (forM_)

import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (isNothing, maybeToList)
import qualified Data.Text.IO         as TextIO

import qualified System.Directory as Dir

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

main :: IO ()
main = do

    -- set up output directories
    Dir.createDirectoryIfMissing True "out"
    Dir.createDirectoryIfMissing True "out/hash"

    -- write the CNAME file so github pages will do its DNS thing
    writeFile "out/CNAME" "chris-martin.org"

    defaultPostCss <- compileCssSource "in/posts/post.scss"

    posts <- getPosts

    indexMarkdown <- TextIO.readFile "in/home/content.md"
    indexCss <- compileCssSource "in/home/home.scss"
    LBS.writeFile "out/index.html" $ renderHtml $
        Home.pageHtml indexMarkdown indexCss posts

    forM_ posts $ \p ->
        writePost $ p { postCss = postCss p ++ (CssCompiled <$> maybeToList defaultPostCss) }

-- todo - use this
resolveThumb :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveThumb path asset =
    do
        outPathMaybe <- writeHashFile fullPath
        when (isNothing outPathMaybe) putError
        return outPathMaybe
    where
    fullPath = path ++ "/" ++ asset
    putError = putStrLn $ "Missing thumbnail: " ++ fullPath
