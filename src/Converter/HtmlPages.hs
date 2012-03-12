module Converter.HtmlPages 
    ( getHtmlPages
    , GetHtmlPagesResult(..)
    , filterOutSections
    , isTopLink
    , containsBaseHref
    , getRootUrl
    ) where 

import Converter.Types
import Converter.Utils (openUrl)
import Converter.Download (downloadPage)

import Text.HTML.TagSoup (parseTags, Tag(..), (~==))
import System.FilePath (takeDirectory, takeFileName, takeExtension, takeBaseName)
import Data.List (nub)

import Test.HUnit

data GetHtmlPagesResult = GetHtmlPagesResult 
    { ghpTocContent      :: String
    , ghpReferencedPages :: [(FilePath, Url)] }

getHtmlPages ::  Url -> IO (Maybe GetHtmlPagesResult)
getHtmlPages tocUrl = do
    maybeToc <- downloadPage tocUrl
    case maybeToc of
        Just toc -> 
            return $ Just $ GetHtmlPagesResult toc $
                ("toc.html", tocUrl) : getNameUrlMap (getFolderUrl tocUrl) toc
        Nothing  -> return Nothing

filterOutSections ::  [String] -> [String]
filterOutSections = filter isTopLink 

isTopLink = notElem '#' 

getNameUrlMap ::  String -> String -> [(String, String)]
getNameUrlMap rootUrl = map (\x -> (x, rootUrl ++ "/" ++ x)) . getHtmlNamesInRootFolder 

getHtmlNamesInRootFolder = getSameFolderHtmls . filterHrefs . parseTags
           
getSameFolderHtmls = nub . filterLocalLinks . getLinks
    where
        filterLocalLinks = filter (notElem '/') 
        -- filterLocalLinks = filter (not . ("http" `isPrefixOf`))

        getLinks ::  [Tag String] -> [String]
        getLinks = map (snd . getUrl)
            where getUrl (TagOpen tag urls) = head urls


filterHrefs ::  [Tag String] -> [Tag String]
filterHrefs = filter (~== "<a href>") 

containsBaseHref :: Line -> Bool
containsBaseHref = (/=[]) . filter (~== "<base href>") . parseTags

getFolderUrl :: Url -> Url
getFolderUrl = takeDirectory

-- | drop "http://" then gobble everything up to first / and stick it onto "http://"
getRootUrl :: Url -> Url
getRootUrl url = http ++ (takeWhile (not . (=='/')) . drop (length http)) url
    where http ="http://"

containsBaseHrefTests = 
    [ assertBool "containsBaseHref" $
         containsBaseHref lineContainingHref
    , assertBool "contains no BaseHref" $
        (not . containsBaseHref) lineContainingNoHref
    ]
    where lineContainingHref = "<base href=\"http://learnyouahaskell.com/\">"
          lineContainingNoHref = "<a href=\"whatever.com\" />"

getFolderUrlTests =
    [ assertEqual "get folder url with html file"
        tocFolderUrl (getFolderUrl tocUrl)
    , assertEqual "get folder url without html file"
        tocFolderUrl (getFolderUrl tocUrlNoHtml)
    ]
    where
        tocUrl  = "http://the.root/pages/read/index.html"
        tocUrlNoHtml  = "http://the.root/pages/read/"
        tocFolderUrl = "http://the.root/pages/read"
         
getRootUrlTests =
    map (\(url, root) -> assertEqual url root (getRootUrl url))
    [ ("http://root.com", "http://root.com")
    , ("http://root.com/pages", "http://root.com")
    , ("http://root.com/pages/index.html", "http://root.com")
    , ("http://root.com/pages/chapter1/toc.htm", "http://root.com")
    ]

tests = TestList $ map TestCase $
    containsBaseHrefTests ++
    getFolderUrlTests ++
    getRootUrlTests
    

runTests = runTestTT tests
