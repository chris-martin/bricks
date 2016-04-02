{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg (main) where

import           ChrisMartinOrg.Core
import           ChrisMartinOrg.Css
import qualified ChrisMartinOrg.Home as Home
import           ChrisMartinOrg.Post (getPosts, writePost)

import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe           (maybeToList)
import qualified Data.Text.Lazy.IO    as LTextIO

import qualified System.Directory as Dir

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

main :: IO ()
main = do

    -- setup up output directories
    Dir.createDirectoryIfMissing True "out"
    Dir.createDirectoryIfMissing True "out/hash"

    -- write the CNAME file so github pages will do its DNS thing
    writeFile "out/CNAME" "chris-martin.org"

    defaultPostCss <- compileCssSource "in/posts/post.scss"

    posts <- getPosts

    indexMarkdown <- LTextIO.readFile "in/home/content.md"
    indexCss <- compileCssSource "in/home/home.scss"
    LBS.writeFile "out/index.html" $ renderHtml $
        Home.pageHtml indexMarkdown indexCss posts

    mapM_ (\p -> writePost $ p { postCss = postCss p ++ (CssCompiled <$> maybeToList defaultPostCss) }) posts
