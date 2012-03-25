module Handler.Convert where

import Import

getConvertR :: Handler RepJson
getConvertR = do
    url <- runInputGet $ ireq textField "urlText"
    liftIO . print $ "Converting"
    jsonToRepJson . object . toTextPairs $ [("converted", "the converted")] 
