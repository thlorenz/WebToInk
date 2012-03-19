{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import System.IO


data WebToKindleServer = WebToKindleServer 
instance Yesod WebToKindleServer 

mkYesod "WebToKindleServer" [parseRoutes|
    /               IndexR GET
    /?String        TestR GET
    /stylesheets/#String StyleR GET 
|]

getIndexR = do
    content <- liftIO $ readFile "public/index.html"    
    return $ RepHtml $ toContent content

getStyleR fileName = do
    content <- liftIO $ readFile $ "public/stylesheets/" ++ fileName
    return $ RepPlain $ toContent content

getTestR qry = do
    liftIO $ print qry
    content <- liftIO $ readFile "public/index.html"    
    return $ RepHtml $ toContent content

main = warpDebug 3000 WebToKindleServer 
