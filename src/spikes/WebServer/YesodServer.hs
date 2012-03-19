{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import System.IO


data WebToKindleServer = WebToKindleServer 
instance Yesod WebToKindleServer 

mkYesod "WebToKindleServer" [parseRoutes|
    / IndexR GET
|]

getIndexR = do
    content <- liftIO $ readFile "public/index.html"    

    return $ RepPlain $ toContent $ show content

main = warpDebug 3000 WebToKindleServer 
