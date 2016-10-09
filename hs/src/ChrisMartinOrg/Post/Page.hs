{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Post.Page
    ( Input(..)
    , html
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Css  (styleLink)

import System.FilePath.Posix ((</>))

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Input = Input
    { inputTitle :: Html
    , inputChron :: Chron
    , inputCss   :: Maybe CompiledCss
    , inputBody  :: Html
    }

html :: Input -> Html
html i = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ inputTitle i
        H.link ! A.rel "icon" ! A.href ""
        mapM_ (styleLink . CompiledCss . (".." </>) . compiledCssPath) $ inputCss i
    H.body $ do
        globalPageHeader PostPage
        H.main $
            H.div ! A.class_ "container" $ do
                H.div ! A.class_ "post-head" $ do
                    H.h1 ! A.class_ "post-title" $
                        inputTitle i
                    H.div ! A.class_ "post-date" $
                        toHtml $ formatChron $ inputChron i
                H.div ! A.class_ "post-body" $ inputBody i
