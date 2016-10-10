{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Home
    ( pageHtml
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Css
import ChrisMartinOrg.Post (postUrl)

import Control.Monad (forM_)

import           Data.String    (IsString (..))
import qualified Data.Text.Lazy as L
import qualified Data.Text      as T

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

pageHtml :: T.Text -> Maybe CompiledCss -> [Post] -> Html
pageHtml md css posts = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.meta ! A.content "width=device-width,initial-scale=1"
               ! A.name "viewport"
        H.title "Chris Martin"
        H.link ! A.rel "icon" ! A.href ""
        mapM_ styleLink css
    H.body $ do
        globalPageHeader HomePage
        H.main $
            H.div ! A.class_ "container" $ do
                H.div $
                    markdown md
                H.div $ do
                    H.h2 "Writings"
                    mapM_ postHtml posts

postHtml :: Post -> Html
postHtml post = H.div ! A.class_ "post" $ do
    H.div ! A.class_ "post-head" $ do
        H.a ! A.class_ "post-title" ! A.href (fromString $ postUrl post) $
            toHtml $ postTitle post
        H.div ! A.class_ "post-date" $
            toHtml $ formatChron $ postChron post
    H.div ! A.class_ "post-abstract" $ do
        forM_ (postThumb post) $ \t ->
            H.img ! A.class_ "post-thumb" ! A.src (fromString t)
        markdown $ postAbstract post
    H.br
