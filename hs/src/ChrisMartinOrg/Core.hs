module ChrisMartinOrg.Core
    ( markdown
    ) where

import Data.Default

import qualified Data.Text.Lazy   as L
import           Text.Blaze.Html5 (Html)
import qualified Text.Markdown    as Markdown

markdown :: L.Text -> Html
markdown = Markdown.markdown def
