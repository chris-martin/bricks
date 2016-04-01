module ChrisMartinOrg.Core
    ( markdown
    , hash
    ) where

import Data.Default

import qualified Crypto.Hash            as Hash
import qualified Crypto.Hash.Algorithms as HashAlg
import           Data.ByteString        (ByteString)
import qualified Data.Text.Lazy         as L
import           Text.Blaze.Html5       (Html)
import qualified Text.Markdown          as Markdown

markdown :: L.Text -> Html
markdown = Markdown.markdown def { Markdown.msXssProtect = False }

hash :: ByteString -> String
hash bs = take 32 $ show ((Hash.hash bs) :: Hash.Digest HashAlg.SHA3_256)
