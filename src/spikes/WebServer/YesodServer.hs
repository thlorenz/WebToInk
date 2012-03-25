{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import System.IO
import Text.Html (URL)
import Control.Applicative ((<$>), (<*>))


data WebToInkServer = WebToInkServer 

instance Yesod WebToInkServer 

instance RenderMessage WebToInkServer FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "WebToInkServer" [parseRoutes|
    /                           RootR GET
    /stylesheets/#String        StyleR GET 
    /title                      TitleR GET 
|]

getTitleR = do
    url <- runInputGet $ ireq textField "urlInput"
    liftIO . print $ "Got tile request"
    liftIO . getTitle . show $ url
    >>= return . RepPlain . toContent

getTitle url = do
    return $ "Url: " ++ url ++ " Title: some title"

getRootR = defaultLayout $ do
    [whamlet|
        <div>
            <h2>WebToInk
            <div id="main">
            <input id="urlInput" type=url placeholder="Enter Url">
    |]
    toWidget [julius|
        onload = function() {
            console.log("Document loaded, hooking up events");
            var urlInput = document.getElementById("urlInput");
            urlInput.onkeyup = function(event) {

                var keycode = (event.keyCode ? event.keyCode : event.which);

                if (keycode == '13') {
                    var request = new XMLHttpRequest();
                    var params = "?urlInput=" + encodeURI(urlInput.value);
                    request.open("GET", "@{TitleR}" + params);
                    request.onreadystatechange = function(reqEvent) {
                        var response = reqEvent.target.responseText;
                        if(response != "") {
                            // When we got a valid response, we are done
                            request.onreadystatechange = null;

                            console.log("Title: ", response);
                            var textNode = document.createTextNode(response);
                            var main = document.getElementById("main");
                            main.appendChild(textNode);
                        }
                    }

                    request.send(null);
                }
            } 
        }
|]

getStyleR fileName = do
    content <- liftIO $ readFile $ "public/stylesheets/" ++ fileName
    return $ RepPlain $ toContent content

main = warpDebug 3000 WebToInkServer 
