module Handler.AboutUs where

import Import

getAboutUsR :: Handler RepHtml
getAboutUsR = defaultLayout $ do
        h2id <- lift newIdent
        setTitle "WebToInk AboutUs"
        $(widgetFile "aboutUs")
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
