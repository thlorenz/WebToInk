module HtmlPages (getHtmlPages, filterOutSections, isTopLink, containsBaseHref) where 

import Types
import Utils(openUrl)
import Download(downloadPage)

import Text.HTML.TagSoup(parseTags, Tag(..), (~==))
import System.FilePath(takeFileName, takeExtension, takeBaseName)
import Data.List(nub)

import Test.HUnit

getHtmlPages ::  Url -> Url -> IO [(FilePath, Url)]
getHtmlPages tocUrl rootUrl = do
    toc <- downloadPage tocUrl
    return $ [("toc.html", tocUrl)] ++ (getNameUrlMap rootUrl toc)

filterOutSections ::  [String] -> [String]
filterOutSections = filter isTopLink 

isTopLink = not . any(=='#')

getNameUrlMap ::  String -> String -> [(String, String)]
getNameUrlMap rootUrl = (map (\x -> (x, rootUrl ++ "/" ++ x))) . getHtmlNamesInRootFolder 

getHtmlNamesInRootFolder = getSameFolderHtmls . filterHrefs . parseTags
           
getSameFolderHtmls = nub . filterLocalLinks . getLinks
    where
        filterLocalLinks = filter (not . any(=='/'))

        getLinks ::  [Tag String] -> [String]
        getLinks = map (snd . getUrl)
            where getUrl (TagOpen tag urls) = head urls


filterHrefs ::  [Tag String] -> [Tag String]
filterHrefs = filter (~== "<a href>") 

containsBaseHref :: Line -> Bool
containsBaseHref = (/=[]) . filter (~== "<base href>") . parseTags

containsBaseHrefTests = 
    [ assertBool "containsBaseHref" $
         containsBaseHref lineContainingHref
    , assertBool "contains no BaseHref" $
        (not . containsBaseHref) lineContainingNoHref
    ]
    where lineContainingHref = "<base href=\"http://learnyouahaskell.com/\">"
          lineContainingNoHref = "<a href=\"whatever.com\" />"

tests = TestList $ map TestCase $
    containsBaseHrefTests 

runTests = do
    runTestTT tests
