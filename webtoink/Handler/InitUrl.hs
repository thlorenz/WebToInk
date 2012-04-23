module Handler.InitUrl where

import Import

import Handler.Utils (getStringFromField, toTextPairs)
import WebToInk.Converter.ConverterService (getTitle)

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- getStringFromField "urlText"

    -- TODO: Although error handling is in place here, the converter service fails to propagate certain errors that could occurr.
    -- E.g., Status: 500 errors are ignored.
    response <- liftIO (tryGetTitle url)
    jsonToRepJson . object . toTextPairs $ response 

tryGetTitle url = do
    result <- getTitle $ url
    return $ case result of
        Right title -> [("title", title), ("url", url)] 
        Left  err   -> [("error", err)]
