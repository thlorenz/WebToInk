import Network.HTTP
import Text.HTML.TagSoup
import Data.List(tails)

main = do 
    src <- findAllPages
    let result = extractLinks src
    return result

-- parseTags :: StringLike str => str -> [Tag str]
extractLinks src = map extractLink hrefs 
    where
        hrefs = [x | x <- parseTags src, x ~== "<a href>"]
        extractLink (TagOpen tag xs) = (snd . head) xs

findAllPages = readFile "index.html" -- openUrl url
    where
        url = "http://book.realworldhaskell.org/read/"


{--
src <- findAllPages
let tags = parseTags src
let hrefs = [x | x <- tags, x ~== "<a href>"]
let fh = head hrefs
--}



-- getResponseBody ::  Network.Stream.Result (Response ty) -> IO 
-- getRequest ::  String -> Request_String
-- simpleHTTP :: HStream ty =>Request ty -> IO (Network.Stream.Result (Response ty))
openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody
