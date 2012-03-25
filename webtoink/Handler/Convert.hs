module Handler.Convert where

import Import

import Data.Text (unpack)

import Handler.Utils (getStringFromField, toTextPairs)
import WebToInk.Converter.ConverterService

getConvertR :: Handler RepJson
getConvertR = do
    url     <- getStringFromField "urlText"
    title   <- getStringFromField "titleText"
    author  <- getStringFromField "authorText"
    liftIO . print $ "Converting"
    path <- liftIO $ getMobi url title author
    jsonToRepJson . object . toTextPairs $ [("converted", "the converted")] 
