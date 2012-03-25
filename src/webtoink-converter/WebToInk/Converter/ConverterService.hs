module WebToInk.Converter.ConverterService where

import Import

import System.Cmd (rawSystem)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.FilePath((<.>))

import WebToInk.Converter.Types
import WebToInk.Converter.Download (downloadPage)
import WebToInk.Converter.HtmlPages (resolveTitle)
import WebToInk.Converter.Converter (prepareKindleGeneration)

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
-- Then generated a .mobi file from it using the kindlegen tool
-- Finally it returns the path to the generated mobi file from which it can be downloaded.
getMobi :: Url -> String -> String -> IO FilePath
getMobi url title author = do
    -- TODO: wrap all this inside try catch
    currentDir <- getCurrentDirectory

    putStrLn $ "Preparing " ++ title ++ " by " ++ author
    path <- prepareKindleGeneration (Just title) (Just author) "en-us" url "../books"
    setCurrentDirectory path
    rawSystem "kindlegen" [ "-o", title<.>"mobi", "book.opf" ]

    setCurrentDirectory currentDir 
    return path
    
