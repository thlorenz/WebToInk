module HtmlPages (getHtmlPages) where 

import Network.HTTP
import Text.HTML.TagSoup
import Text.StringLike
import System.FilePath
import Data.List

getHtmlPages tocUrl = do
    toc <- getIndexContents tocUrl
    return (getNameUrlIndex tocUrl toc)

getNameUrlIndex ::  [Char] -> [Char] -> [([Char], [Char])]
getNameUrlIndex tocUrl = (map (\x -> (x, tocUrl ++ x))) . getHtmlNamesInRootFolder 

getHtmlNamesInRootFolder = getSameFolderHtmls . filterHrefs . parseTags

getSameFolderHtmls = nub . filterHtmls . filterLocalLinks . getLinks
    where
        filterLocalLinks = filter (not . any(=='/'))
        filterHtmls = filter isHtmlFile
            where isHtmlFile x = let extension = takeExtension x
                                 in  extension == ".html" || extension == ".htm"

        getLinks ::  [Tag String] -> [String]
        getLinks = map (snd . getUrl)
            where getUrl (TagOpen tag urls) = head urls

filterHrefs ::  [Tag [Char]] -> [Tag [Char]]
filterHrefs = filter (~== "<a href>") 

getIndexContents url = readFile "index.html" -- openUrl url

openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody
