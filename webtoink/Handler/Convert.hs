module Handler.Convert where

import Import

import System.FilePath (combine)
import WebToInk.Converter.ConverterService

import Handler.Utils (getStringFromField, toTextPairs)
import Settings (staticDir)


getConvertR :: Handler RepJson
getConvertR = do
    url     <- getStringFromField "urlText"
    title   <- getStringFromField "titleText"
    author  <- getStringFromField "authorText"
    liftIO . putStrLn $ "Converting"
    path <- liftIO $ getMobi url title author (combine staticDir "books")
    jsonToRepJson . object . toTextPairs $ [("fileType", "mobi"), ("path", path)] 
