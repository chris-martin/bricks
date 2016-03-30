{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module ChrisMartinOrg (main) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Post

import qualified Crypto.Hash            as Hash
import qualified Crypto.Hash.Algorithms as HashAlg

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Default
import           Data.String          (IsString (..))
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as LTextIO

import qualified System.Directory as Dir

import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              as H hiding (main)
import qualified Text.Blaze.Html5.Attributes   as A

import qualified Text.Sass             as Sass
import qualified Text.Sass.Compilation as SassC
import           Text.Sass.Options     (SassOptions (..), SassOutputStyle (..))

main :: IO ()
main = do

    -- setup up output directories
    Dir.createDirectoryIfMissing True "out"
    Dir.createDirectoryIfMissing True "out/css"

    -- write the CNAME file so github pages will do its DNS thing
    writeFile "out/CNAME" "chris-martin.org"

    posts <- getPosts

    indexMarkdown <- LTextIO.readFile "in/index/content.md"
    indexCss <- compileCss "in/index/style.scss"
    LBS.writeFile "out/index.html" $ renderHtml $
        indexHtml indexMarkdown indexCss posts

indexHtml :: L.Text -> Maybe FilePath -> [Post] -> Html
indexHtml md stylePath posts = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Chris Martin"
        mapM_ styleLink stylePath
    H.body $ do
        H.div ! A.class_ "container" $ do
            H.div $ markdown md
            H.h2 "Writings"
            mapM_ postHtml posts

postHtml :: Post -> Html
postHtml post = H.div ! A.class_ "post" $ do
    H.div ! A.class_ "head" $ do
        H.a ! A.class_ "title" ! A.href (H.textValue $ postUrl post) $
            postTitle post
        H.div ! A.class_ "date" $
            toHtml $ formatChron $ postChron post
    H.div ! A.class_ "abstract" $
        postAbstract post

postUrl :: Post -> T.Text
postUrl p = T.concat
    [ T.pack $ show $ chronYear $ postChron p
    , "/"
    , postSlug p
    ]

compileCss :: FilePath -> IO (Maybe FilePath)
compileCss inFile = do
    result <- Sass.compileFile inFile sassOpts
    case result of
        Left err -> do
            SassC.errorMessage err >>= putStrLn
            return Nothing
        Right bs -> do
            let outFile = "css/" ++ (hash bs)
            BS.writeFile ("out/" ++ outFile) bs
            return $ Just outFile

sassOpts :: SassOptions
sassOpts = def { sassOutputStyle = SassStyleCompact }

styleLink :: FilePath -> Html
styleLink href = link
    ! A.rel "stylesheet"
    ! A.type_ "text/css"
    ! A.href (fromString href)

hash :: ByteString -> String
hash bs = take 32 $ show ((Hash.hash bs) :: Hash.Digest HashAlg.SHA3_256)
