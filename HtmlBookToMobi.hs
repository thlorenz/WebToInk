import HtmlPages(openUrl, getHtmlPages)
import Download(downloadPages)

import System.FilePath(dropExtension)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory)

main = do 
    let url = "http://book.realworldhaskell.org/read/"
    dic <- getHtmlPages url

    let folder = "real-haskell-book"
    createDirectoryIfMissing False folder 
    setCurrentDirectory folder
    -- downloadPages dic
    setCurrentDirectory ".."

    let xmlItems = map (\(fileName, _) -> xmlForItem "pages" fileName) dic
    putStrLn $ unlines xmlItems

xmlForItem folder fileName =
    "<item id=\"" ++ itemName ++ "\" " ++ "media-type=\"application/xhtml+xml\" href=\"" ++ folder ++ "/" ++ fileName ++"\" />"
    where itemName = dropExtension fileName

