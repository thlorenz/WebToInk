module Handler.AboutUs where

import Import

getAboutUsR :: Handler RepHtml
getAboutUsR = defaultLayout $ do
        h2id <- lift newIdent
        setTitle "WebToInk AboutUs"
        $(widgetFile "aboutUs")
