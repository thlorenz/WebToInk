module Handler.Convert where

import Import

getConvertR :: Handler RepJson
getConvertR = do
    url <- runInputGet $ ireq textField "readOnlyUrlText"
    jsonToRepJson . object . toTextPairs $ [("converted", "the converted")] 
