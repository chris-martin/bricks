{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg (main) where

import           ChrisMartinOrg.Css
import qualified ChrisMartinOrg.Home as Home
import           ChrisMartinOrg.Post (Post, getPosts, postUrl)
import qualified ChrisMartinOrg.Post as Post

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Lazy.IO    as LTextIO

import Control.Applicative ((<|>))
import Control.Monad       (join)

import qualified System.Directory      as Dir
import           System.FilePath.Posix (dropFileName)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

main :: IO ()
main = do

    -- setup up output directories
    Dir.createDirectoryIfMissing True "out"
    Dir.createDirectoryIfMissing True "out/css"

    -- write the CNAME file so github pages will do its DNS thing
    writeFile "out/CNAME" "chris-martin.org"

    posts <- getPosts

    defaultPostCss <- compileCss "in/posts/post.scss"

    indexMarkdown <- LTextIO.readFile "in/home/content.md"
    indexCss <- compileCss "in/home/home.scss"
    LBS.writeFile "out/index.html" $ renderHtml $
        Home.pageHtml indexMarkdown indexCss (snd <$> posts)

    mapM_ (\p -> writePost p defaultPostCss) (snd <$> posts)

writePost :: Post -> Maybe FilePath -> IO ()
writePost post defaultCss = do
    Dir.createDirectoryIfMissing True $ dropFileName path
    postCss <- join <$> sequence (compileCss <$> Post.postCss post)
    let html = renderHtml $ Post.pageHtml post (postCss <|> defaultCss)
    LBS.writeFile path html
  where
    path = "out/" ++ (T.unpack $ postUrl post)
