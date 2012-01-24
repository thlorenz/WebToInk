module Download(downloadPages) where

import Utils(openUrl)
import HtmlPages(getHtmlPages)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory)
import System.IO(hPutStr, withFile, IOMode(..))

downloadPages ::  [(FilePath, String)] -> IO ()
downloadPages dic = do
    let pagesFolder = "pages"
    createDirectoryIfMissing False pagesFolder
    setCurrentDirectory pagesFolder 
    mapM downloadPage dic
    setCurrentDirectory ".."

downloadPage ::  (FilePath, String) -> IO ()
downloadPage (fileName, url) = do
    pageContents <- openUrl url
    write fileName pageContents 
    where write fileName pageContents = do 
            withFile fileName WriteMode (\handle -> hPutStr handle pageContents)

