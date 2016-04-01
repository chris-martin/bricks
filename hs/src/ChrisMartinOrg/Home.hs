{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Home
    ( pageHtml
    ) where

import ChrisMartinOrg.Chron
import ChrisMartinOrg.Core
import ChrisMartinOrg.Css
import ChrisMartinOrg.Post  (Post (..), postUrl)

import qualified Data.Text.Lazy as L

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

pageHtml :: Foldable t => L.Text -> t FilePath -> [Post] -> Html
pageHtml md stylePath posts = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Chris Martin"
        mapM_ styleLink stylePath
    H.body $ do
        H.div ! A.class_ "container" $ do
            H.section $ markdown md
            H.section $ do
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
        markdown $ L.fromStrict $ postAbstract post
