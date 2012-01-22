import Network.HTTP
import Text.HTML.TagSoup

main = do 
    src <- findAllPages
    let result = extractLinks src
    return result

extractLinks src = map extractLink hrefs 
    where
        hrefs = [x | x <- parseTags src, x ~== "<a href>"]
        extractLink (TagOpen tag xs) = (snd . head) xs

findAllPages = readFile "index.html" -- openUrl url
    where
        url = "http://book.realworldhaskell.org/read/"

openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody
