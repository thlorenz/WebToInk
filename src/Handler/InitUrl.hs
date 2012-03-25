module Handler.InitUrl where

import Import

import Data.Text (unpack)

import Converter.ConverterService (getTitle)

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- fmap unpack . runInputGet $ ireq textField "urlText"
    title <- liftIO . getTitle $ url
    jsonToRepJson . object . toTextPairs $ 
        [ ("title"  , title)
        , ("url"    , url)
        ] 
