import HtmlPages(getHtmlPages)

main = do 
    let url = "http://book.realworldhaskell.org/read/"
    dic <- getHtmlPages url
    putStrLn (show dic)


