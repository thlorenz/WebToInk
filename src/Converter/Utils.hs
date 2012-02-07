module Converter.Utils(openUrl, getTabs) where

import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Control.Monad.IO.Class (MonadIO)

openUrl :: String -> IO String
openUrl url = do
    bytes <- simpleHttp url 
    return $ U.toString bytes

getTabs indent = replicate (indent * 2) ' '
