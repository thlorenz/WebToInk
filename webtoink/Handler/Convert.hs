module Handler.Convert where

import Import

import Data.Text (unpack)

import WebToInk.Converter.ConverterService

getConvertR :: Handler RepJson
getConvertR = do
    url <- fmap unpack . runInputGet $ ireq textField "urlText"
    title <- fmap unpack . runInputGet $ ireq textField "titleText"
    author <- fmap unpack . runInputGet $ ireq textField "authorText"
    liftIO . print $ "Converting"
    path <- liftIO $ getMobi url title author
    jsonToRepJson . object . toTextPairs $ [("converted", "the converted")] 
