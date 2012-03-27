module Handler.Book where

import Import

import Data.ByteString (ByteString)
import System.FilePath (combine)

import Handler.Utils (getStringFromField, toTextPairs)
import Settings (publicDir)

getBookR :: FilePath -> FilePath -> Handler RepPlain
getBookR folder fileName = do 
    $(logInfo) "Serving: " -- ++ (show . combine $ folder fileName)
    return $ RepPlain $ toContent ("Hello" :: ByteString)

