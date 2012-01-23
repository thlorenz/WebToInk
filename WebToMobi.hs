import HtmlPages(openUrl, getHtmlPages)
import Download(downloadPages)

import System.Directory(createDirectoryIfMissing, setCurrentDirectory)

main = do 
    let url = "http://book.realworldhaskell.org/read/"
    dic <- getHtmlPages url

    let folder = "real-haskell-book"
    createDirectoryIfMissing False folder 
    setCurrentDirectory folder
    downloadPages dic

