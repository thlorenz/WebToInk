module WebToInk.Converter.ConverterService where

import System.Cmd (rawSystem)
import System.Posix.Files (setFileMode, unionFileModes, ownerModes, otherExecuteMode)
import System.FilePath(combine, (<.>))

import WebToInk.Converter.Types
import WebToInk.Converter.Download (downloadPage)
import WebToInk.Converter.HtmlPages (resolveTitle)
import WebToInk.Converter (prepareKindleGeneration)

-- | Tries to download page at given url and resolve title.
-- If anything goes wrong an empty string is returned.
getTitle :: Url -> IO String
getTitle url = do 
  maybeToc <- downloadPage url
  return $ case maybeToc of
      Just toc -> resolveTitle Nothing toc
      Nothing  -> ""

-- | Resolves page at url and all direct children.
-- Downloads all the pages and their images.
-- Then generates a .mobi file from it using the kindlegen tool
-- Finally it returns the path to the generated mobi file from which it can be downloaded.
getMobi :: Url -> String -> String -> FilePath -> IO FilePath
getMobi url title author targetFolder = do
    
    -- TODO: wrap all this inside try catch

    putStrLn $ "Preparing " ++ title ++ " by " ++ author
    path <- prepareKindleGeneration (Just title) (Just author) "en-us" url targetFolder

    -- Allow all users to enter path and read from it since we want to make this available
    -- TODO: handle the case where current user is not permitted to change permissions
    setFileMode path $ unionFileModes ownerModes otherExecuteMode

    rawSystem "kindlegen" [ "-o", targetFile, combine path "book.opf" ]

    return $ combine path targetFile

  where targetFile = title<.>"mobi"
    
main = getMobi url title author targetFolder
  where 
    url = "http://thorstenlorenz.wordpress.com/2012/03/02/lion-logging-to-growl-messages-from-haskell-using-hslogger-an-growlnotify/"
    title = "Logging to Growl from Haskell running on Lion « Thorsten Lorenz"
    author = "Thorsten Lorenz"
    targetFolder = "../books"
