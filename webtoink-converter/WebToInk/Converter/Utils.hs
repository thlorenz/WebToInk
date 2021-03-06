module WebToInk.Converter.Utils( openUrl
                                , downloadByteString
                                , getTabs
                                , cleanFolderName
                                ) where

import Network.HTTP.Conduit (simpleHttp, HttpException(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U

import Control.Monad.IO.Class (MonadIO)
import Data.List.Utils (replace)

import Control.Exception (try, Exception)

import WebToInk.Converter.Types
import WebToInk.Converter.Logger

openUrl :: String -> IO (Maybe String)
openUrl url = do
    bytes <- downloadByteString url 
    case bytes of
        Just bytes -> return $ Just (U.toString bytes)
        Nothing    -> return Nothing

downloadByteString :: Url -> IO (Maybe L.ByteString)
downloadByteString url = do
    byteString <- try (simpleHttp url) :: (Exception a) => IO (Either a L.ByteString)
    case byteString of
        Right x                                   -> return (Just x)
        Left (StatusCodeException status headers) ->
            logw ("An error occured while trying to download: " ++ url)
            >> logw (show status) >> return Nothing
        Left (InvalidUrlException status headers) -> 
            logw ("An error occured while trying to download: " ++ url)
            >> logw (show status) >> logw (show headers) >> return Nothing
        Left a                                    -> 
            logw ("An error occured while trying to download: " ++ url)
            >> logw (show a) >> return Nothing

getTabs indent = replicate (indent * 2) ' '

cleanFolderName :: String -> String 
cleanFolderName = replace "/" "_"

