module ChrisMartinOrg.Hash
    ( hash
    , writeHashBS
    , writeHashFile
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified Crypto.Hash            as Hash
import qualified Crypto.Hash.Algorithms as HashAlg

import qualified System.Directory      as Dir
import           System.FilePath.Posix (takeExtension, (<.>), (</>))

hash :: ByteString -> String
hash bs = take 32 $ show ((Hash.hash bs) :: Hash.Digest HashAlg.SHA3_256)

writeHashBS :: ByteString -> String -> IO FilePath
writeHashBS bs ext = do
    BS.writeFile ("out/" ++ url) bs
    return url
  where
    h = hash bs
    url = "hash" </> h <.> ext

writeHashFile :: FilePath -> IO (Maybe FilePath)
writeHashFile file = do
    exists <- Dir.doesFileExist file
    if exists
        then undefined  -- todo
        else pure Nothing
  where
    ext = takeExtension file
