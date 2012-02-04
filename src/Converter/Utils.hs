module Converter.Utils(openUrl, getTabs) where

import Network.HTTP(simpleHTTP, getResponseBody, getRequest)

openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody

getTabs indent = replicate (indent * 2) ' '
