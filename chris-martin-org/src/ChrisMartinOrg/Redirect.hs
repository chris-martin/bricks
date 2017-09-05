{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Redirect
  ( redirectHtml
  ) where

import Data.String (fromString)
import Text.Blaze.Html5 (Html, toHtml, (!))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

redirectHtml :: FilePath -> Html
redirectHtml target =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title "Redirect"
      H.link ! A.rel "icon" ! A.href ""
    H.body $ do
      H.a ! A.href (fromString target) $ toHtml target
