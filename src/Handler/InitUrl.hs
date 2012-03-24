module Handler.InitUrl where

import Import

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- runInputGet $ ireq textField "urlText"
    jsonToRepJson . object . toTextPairs $ [("title", "the title")] 
