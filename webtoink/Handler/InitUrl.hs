module Handler.InitUrl where

import Import

import Handler.Utils (getStringFromField, toTextPairs)
import WebToInk.Converter.ConverterService (getTitle)

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- getStringFromField "urlText"
    title <- liftIO . getTitle $ url
    jsonToRepJson . object . toTextPairs $ 
        [ ("title"  , title)
        , ("url"    , url)
        ] 
