module Handler.InitUrl where

import Import

getInitUrlR :: Handler RepHtml
getInitUrlR = do
    url <- runInputGet $ ireq textField "urlText"
    liftIO . print $ " -------- Serving title request ----- for: " ++ (show url)
    defaultLayout [whamlet|<p>Yay|]
    {- 
    return . RepPlain $ "{ 'title': 'some title' }"
    -}
