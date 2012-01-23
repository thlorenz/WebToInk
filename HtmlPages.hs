module HtmlPages (openUrl, getHtmlPages) where 

import Network.HTTP(simpleHTTP, getResponseBody, getRequest)
import Text.HTML.TagSoup(parseTags, Tag(..), (~==))
import System.FilePath(takeExtension)
import Data.List(nub)

openUrl ::  String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody

getHtmlPages ::  String -> IO [(String, String)]
getHtmlPages tocUrl = do
    toc <- getIndexContents tocUrl
    return $ [("toc.html", tocUrl)] ++ (getNameUrlMap tocUrl toc)

getNameUrlMap ::  String -> String -> [(String, String)]
getNameUrlMap tocUrl = (map (\x -> (x, tocUrl ++ x))) . getHtmlNamesInRootFolder 

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
