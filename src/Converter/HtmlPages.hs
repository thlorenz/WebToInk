{-# LANGUAGE NoMonomorphismRestriction #-}

module Converter.HtmlPages 
    ( getHtmlPages
    , GetHtmlPagesResult(..)
    , filterOutSections
    , isTopLink
    , containsBaseHref
    , getRootUrl
    , resolveAuthor
    , resolveTitle 
    , removeScripts
    , removeBaseHref
    , localizeSrcUrls 
    ) where 

import Data.Maybe (fromJust)

import Text.HTML.TagSoup (parseTags, Tag(..), (~==), sections)
import System.FilePath (takeDirectory, takeFileName, takeExtension, takeBaseName)
import Data.List (nub)
import Data.String.Utils (replace)
import Converter.Download (getSrcFilePath)

import Test.HUnit

import Converter.Types
import Converter.Utils (openUrl, cleanFolderName)
import Converter.Download (downloadPage)

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

-- | Returns author if one was given, otherwise tries to resolve it from tocContent
resolveAuthor :: Maybe String -> String -> String
resolveAuthor maybeAuthor tocContent =
    case maybeAuthor of
        Just author    -> author
        Nothing        -> "Web author"

-- | Returns title if one was given, otherwise tries to resolve it from tocContent
resolveTitle :: Maybe String -> String -> String
resolveTitle maybeTitle tocContent = 
    case maybeTitle of
        Just title    -> title
        Nothing       -> cleanFolderName $ resolveSection "<title>" tocContent "N/A"
        
resolveSection sectionName html alternative =
    case tryExtractSection sectionName html of
        Just (TagText text) -> text
        Nothing             -> alternative

tryExtractSection sectionName html = 
    case (>2) . length $ section of 
        True    -> Just $ section !! 1
        False   -> Nothing
  where section = let xs = (sections (~== sectionName) . parseTags) html 
                  in if xs == [] then [] else head xs


filterHrefs ::  [Tag String] -> [Tag String]
filterHrefs = filter (~== "<a href>") 

containsBaseHref :: Line -> Bool
containsBaseHref = (/=[]) . filter (~== "<base href>") . parseTags

getFolderUrl :: Url -> Url
getFolderUrl = takeDirectory

-- | remove all script texts found inside html
removeScripts ::  PageContents -> PageContents
removeScripts htmlWithScripts = go htmlWithScripts scriptTexts
  where scriptTexts    = filterScripts . parseTags $ htmlWithScripts
        go html []     = html
        go html (x:xs) = go (replace x "" html) xs

filterScripts = go [] 
  where 
    go scripts []                                                   = scripts
    go scripts ((TagOpen "script" attributes):(TagText script):xs)  = go (script:scripts) xs
    go scripts (_:xs)                                               = go scripts xs

-- | drop "http://" then gobble everything up to first / and stick it onto "http://"
getRootUrl :: Url -> Url
getRootUrl url = http ++ (takeWhile (not . (=='/')) . drop (length http)) url
    where http ="http://"

localizeSrcUrls :: FilePath -> [Url] -> PageContents  -> PageContents
localizeSrcUrls targetFolder srcUrls pageContents =
    foldr (\srcUrl contents -> 
        replace ("src=\"" ++ srcUrl) ("src=\"" ++ getSrcFilePath targetFolder srcUrl) contents) 
        pageContents srcUrls

removeBaseHref :: PageContents -> PageContents
removeBaseHref = unlines . filter (not . containsBaseHref) . lines
    
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

resolveTitleTests = 
    [ assertEqual "find title when contained and not given" "Ubuntu Server Guide" $
        resolveTitle Nothing htmlContainingTitle 
    ]

  where htmlContainingTitle = 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\">" ++
             "<head xmlns=\"http://www.w3.org/1999/xhtml\">" ++
                 "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />" ++
                 "<title xmlns=\"\">Ubuntu Server Guide</title>" ++
                 "<link rel=\"stylesheet\" href=\"../../libs/ubuntu-book.css\" type=\"text/css\" />" ++
             "</head>" ++
         "</html>"

removeScriptsTests = [ assertEqual "removing scripts from html" htmlWithoutScripts $ 
                        removeScripts htmlWithScripts 
                     ]
  where
    htmlWithScripts = 
        "<body>" ++
            "<script type=\"text/javascript\">some JavaScript here</script>" ++
            "<p>Some paragraph</p>" ++
            "<script type=\"text/javascript\">some more JavaScript here</script>" ++
        "</body>"
    htmlWithoutScripts =
        "<body>" ++
            "<script type=\"text/javascript\"></script>" ++
            "<p>Some paragraph</p>" ++
            "<script type=\"text/javascript\"></script>" ++
        "</body>"

localizeSrcUrlsTests =
    [ assertEqual "localizing src urls"
        (localizeSrcUrls filePath imageUrls pageContents) localizedPageContents 
    ]
    where
        filePath = "../images"
        pageContents = 
            "<body>" ++
                "<img src=\"/support/figs/rss.png\"/>" ++
                "<span>some span</span>" ++
                "<img src=\"/support/figs/ball.png\"/>" ++
            "</body>"
        imageUrls = [ "/support/figs/rss.png", "/support/figs/ball.png" ]
        localizedPageContents =
            "<body>" ++
                "<img src=\"" ++ filePath ++ "/rss.png\"/>" ++
                "<span>some span</span>" ++
                "<img src=\"" ++ filePath ++ "/ball.png\"/>" ++
            "</body>"

removeBaseHrefTests = 
    [ assertEqual "removing base href"
        processedPageContents (removeBaseHref pageContents)
    ]
    where 
        pageContents =
            "<head>\n" ++
                "<base href=\"http://learnyouahaskell.com/\">\n" ++ 
            "</head>\n"
        processedPageContents = 
            "<head>\n" ++
            "</head>\n"
    
tests = TestList $ map TestCase $
    containsBaseHrefTests ++
    getFolderUrlTests ++
    getRootUrlTests ++
    resolveTitleTests ++
    removeScriptsTests ++
    localizeSrcUrlsTests ++
    removeBaseHrefTests 
    

runTests = runTestTT tests
