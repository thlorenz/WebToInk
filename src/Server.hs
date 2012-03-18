{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ( (<$>) )
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as L 

import Network.HTTP.Types (status200)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Blaze.ByteString.Builder (fromLazyByteString, copyByteString)

instance Show Request where
    show r = "\nRequest:" ++
             "\n  Method: " ++ show (requestMethod r) ++
             "\n  HTTPVersion: " ++ show (httpVersion r) ++ 
             "\n  PathInfo: " ++ show (pathInfo r) ++
             "\n  QueryString: " ++ show (queryString r) ++ 
             "\n  ServerName: " ++ show (serverName r) ++
             "\n  ServerPort: " ++ show (serverPort r) ++
             "\n  RequestHeaders: " ++ show (requestHeaders r) ++
             "\n  IsSecure: " ++ show (isSecure r) ++
             "\n  RemoteHost: " ++ show (remoteHost r)
main :: IO ()
main = do
    -- get port from first command arg
    port <- read . fromMaybe "3000" . listToMaybe <$> getArgs
    putStrLn $ "Listening on localhost:" ++ show port
    run port app 

app req = do
    html <- readIndex req
    return html   
         

readIndex ::  Show a => a -> IO Response
readIndex req = do
    print req 
    idx <- readIndex
    return $ ResponseBuilder status200 htmlContentType idx
    where readIndex = fmap fromLazyByteString . L.readFile $ "public/index.html"
          htmlContentType = [ ("Content-Type", "text/html") ] 

{-

data Response
  = ResponseFile Network.HTTP.Types.Status
                 Network.HTTP.Types.ResponseHeaders
                 FilePath
                 (Maybe FilePart)
  | ResponseBuilder Network.HTTP.Types.Status
                    Network.HTTP.Types.ResponseHeaders
                    Blaze.ByteString.Builder.Internal.Types.Builder
  | ResponseSource Network.HTTP.Types.Status
                   Network.HTTP.Types.ResponseHeaders
                   (conduit-0.2.1:Data.Conduit.Types.Source.Source
                      IO
                      (Data.Conduit.Flush
                         Blaze.ByteString.Builder.Internal.Types.Builder))

run :: Port -> Application -> IO ()
-}
