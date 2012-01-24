module Utils(openUrl) where

import Network.HTTP(simpleHTTP, getResponseBody, getRequest)

openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody

