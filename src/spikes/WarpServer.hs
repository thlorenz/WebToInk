{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid (mconcat)

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
main = do
    let port = 3000
    putStrLn $ "Listening on port" ++ show port
    run port app

app ::  Monad m => Request -> m Response
app req = do 
    case pathInfo req of
        ["yay"] ->  return yay
        x       ->  return (index x req)


yay = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ 
        mconcat $ map copyByteString ["yay"]

index x req = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ 
        mconcat $ map copyByteString
         [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
         , "<p>Request was: ", BU.fromString $ show req, "</p>" ]
            


{--         [ "<html>"
            ,   "<body>"
            ,     "<h1>Hello World!</h1>"
            ,   "</body>"
            , "</html>"
            ]
        --}
