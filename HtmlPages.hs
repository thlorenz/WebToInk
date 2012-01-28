module HtmlPages (getHtmlPages) where 

import Text.HTML.TagSoup(parseTags, Tag(..), (~==))
import System.FilePath(takeExtension)
import Data.List(nub)
import Types

getHtmlPages ::  Url -> IO [(FilePath, Url)]
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

filterHrefs ::  [Tag String] -> [Tag String]
filterHrefs = filter (~== "<a href>") 

getIndexContents url = readFile "index.html" -- openUrl url
