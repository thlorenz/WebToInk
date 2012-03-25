{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator

main = do
    req <- parseUrl "http://learnyouahaskell.com/chapters"
    res <- withManager $ httpLbs req { method = "HEAD" }
    print res
