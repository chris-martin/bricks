{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg (main) where

import           ChrisMartinOrg.Core
import           ChrisMartinOrg.Css
import           ChrisMartinOrg.Hash (writeHashFile)
import qualified ChrisMartinOrg.Home as Home
import           ChrisMartinOrg.Post (getPosts, writePost)

import           Control.Monad       (when, forM_)

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (toList)
import           Data.Functor         (($>))
import           Data.Maybe           (isNothing, maybeToList)
import           Data.Monoid          ((<>))
import qualified Data.Text.IO         as TextIO

import qualified System.Directory as Dir

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

defaultPostCssPath, homeContentPath, homeCssPath :: FilePath

defaultPostCssPath = "in/posts/post.scss"
homeContentPath    = "in/home/content.md"
homeCssPath        = "in/home/home.scss"

main :: IO ()
main = do

    -- set up output directories
    Dir.createDirectoryIfMissing True "out"
    Dir.createDirectoryIfMissing True "out/hash"

    -- write the CNAME file so github pages will do its DNS thing
    writeFile "out/CNAME" "chris-martin.org"

    homeCss <- compileCssSource homeCssPath

    ifLeft homeCss (\err ->
        putStrLn $ "Failed to compile the home page CSS "
                 <> defaultPostCssPath <> " - " <> err)

    let homeCssMaybe = either (const Nothing) Just homeCss

    defaultPostCss <- compileCssSource defaultPostCssPath

    let defaultPostCssMaybe = either (const Nothing) Just homeCss

    ifLeft defaultPostCss (\err ->
        putStrLn $ "Failed to compile the default post CSS "
                <> defaultPostCssPath <> " - " <> err)

    posts <- getPosts

    posts' <- sequence (patchPost defaultPostCssMaybe <$> posts)

    homeMarkdown <- TextIO.readFile homeContentPath
    let homeHtml = renderHtml $ Home.pageHtml homeMarkdown homeCssMaybe posts'
    LBS.writeFile "out/index.html" homeHtml

    forM_ posts' $ writePost

ifLeft :: Monad m => Either a b -> (a -> m c) -> m ()
ifLeft e f = either (\x -> f x $> ()) (const $ pure ()) e

patchPost :: Maybe CompiledCss -- ^ Default post css
          -> Post -> IO Post
patchPost defaultPostCssMaybe p = do
    let postCss' = postCss p ++ (CssCompiled <$> toList defaultPostCssMaybe)
    postThumb' <- resolveThumbMaybe $ postThumb p
    return $ p { postCss   = postCss'
               , postThumb = postThumb' }

resolveThumb :: FilePath -> IO (Either String FilePath)
resolveThumb path = do
    outPathMaybe <- writeHashFile path
    return $ case outPathMaybe of
        Nothing -> Left $ "Missing thumbnail: " <> path
        Just t -> Right t

resolveThumbMaybe :: Maybe FilePath -> IO (Maybe FilePath)
resolveThumbMaybe Nothing = return Nothing
resolveThumbMaybe (Just t) = do
    e <- resolveThumb t
    case e of
        Left err -> do putStrLn err
                       return Nothing
        Right t' -> return $ Just t'
