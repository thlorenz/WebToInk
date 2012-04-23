module Handler.Convert where

import Import

import System.FilePath (takeFileName)

import WebToInk.Converter.ConverterService
import Handler.Utils (getStringFromField, toTextPairs)
import Settings (booksDir)

getConvertR :: Handler RepJson
getConvertR = do
    url     <- getStringFromField "urlText"
    title   <- getStringFromField "titleText"
    author  <- getStringFromField "authorText"
    liftIO . putStrLn $ "Converting"
    response <- liftIO (convertMobi url title author)
    jsonToRepJson . object . toTextPairs $ response

convertMobi url title author = do
    result <- getMobi url title author booksDir
    return $ case result of
        Right path  -> [("fileName", takeFileName path), ("fileType", "mobi"), ("path", path)] 
        Left  err ->   [("error", err)]

