module Handler.Root where

import Import

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "WebToInk homepage"
    $(widgetFile "homepage")
    addStylesheet $ StaticR css_homepage_css
    addScript $ StaticR js_spin_min_js
