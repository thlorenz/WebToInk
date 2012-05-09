module WebToInk.Converter.Utils( openUrl
                                , downloadByteString
                                , getTabs
                                , cleanFolderName
                                , initLogger
                                , logd
                                , logi
                                , logw
                                , loge
                                , logt
                                ) where

import Network.HTTP.Conduit (simpleHttp, HttpException(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Char (toUpper)

import Control.Monad (when)

import Control.Monad.IO.Class (MonadIO)
import Data.List.Utils (replace)

import Control.Exception (try, Exception)

import WebToInk.Converter.Types

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
            putStrLn ("An error occured while trying to download: " ++ url)
            >> print status >> return Nothing
        Left (InvalidUrlException status headers) -> 
            putStrLn ("An error occured while trying to download: " ++ url)
            >> print status >> print headers >> return Nothing
        Left a                                    -> 
            putStrLn ("An error occured while trying to download: " ++ url)
            >> print a >> return Nothing

getTabs indent = replicate (indent * 2) ' '

cleanFolderName :: String -> String 
cleanFolderName = replace "/" "_"

-- Logging 
tracing = True
loggerName = "Converter"

initLogger ::  String -> Maybe FilePath -> IO ()
initLogger p mbFilePath =
    updateGlobalLogger loggerName (setLevel prio) 
    >>  case mbFilePath of
            Nothing     -> return ()
            Just path   -> do
                h <- fileHandler path DEBUG >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "[$time $loggername  $prio\t$tid] $msg")
                updateGlobalLogger loggerName (addHandler h)
    where prio = stringToPriority p
          stringToPriority = read . map toUpper :: String -> Priority


logd ::  String -> IO ()
logd = debugM loggerName

logi ::  String -> IO ()
logi = infoM  loggerName

logw ::  String -> IO ()
logw = warningM loggerName

loge ::  String -> IO ()
loge = errorM loggerName

logt ::  String -> IO ()
logt msg = when tracing (logd msg)
