import HtmlPages(getHtmlPages, filterOutSections, isTopLink, containsBaseHref)
import Images(getImages)
import Download(downloadPage, savePage, downloadAndSaveImages, getSrcFilePath)
import OpfGeneration(generateOpf)
import TocGeneration(generateToc)
import CommandLineParser(Args(..), legend, parseArgs)
import Types
import Constants

import System.Directory(createDirectoryIfMissing, setCurrentDirectory)
import Control.Monad(forM)
import Data.String.Utils(replace)
import Data.Maybe(fromJust)
import System.Environment(getArgs)

import Test.HUnit
import List(nub)

main = do 
    argsList <- getArgs

    -- TODO: check args to be valid
    let args = parseArgs argsList

    prepareKindleGeneration 
        (fromJust $ title args)
        (fromJust $ author args) 
        (language args) 
        (fromJust $ tocUrl args) 
        (fromJust $ rootUrl args)


prepareKindleGeneration title creator language tocUrl rootUrl = do

    -- we need to use root, tocRoot, pagesRoot, imagesRoot etc.
    -- root defaults to tocRoot
    -- pagesRoot defaults to tocRoot
    -- imagesRoot defaults to root and also uses tocRoot
    pagesDic <- getHtmlPages tocUrl

    let topPagesDic = filter (isTopLink . fst) pagesDic
    let topPages = map fst topPagesDic

    putStrLn $ prettifyList topPagesDic
    
    -- createKindleStructure topPagesDic topPages

    where
        createKindleStructure topPagesDic topPages = do

            createDirectoryIfMissing False title  
            setCurrentDirectory title

            referencedImages <- downloadPages rootUrl tocUrl topPagesDic

            putStrLn $ prettifyList topPages 
            let opfString = generateOpf topPages referencedImages title language creator 
            writeFile "book.opf" opfString

            let tocString = generateToc topPages title language creator
            writeFile "toc.ncx" tocString

            setCurrentDirectory ".."

downloadPages rootUrl tocUrl topPagesDic = do
    allImageUrls <- mapM (\(fileName, url) -> do
        putStrLn $ "Downloading: " ++ fileName
        pageContents <- downloadPage url

        let imageUrls = getImages pageContents

        putStrLn $ prettifyList imageUrls 
        downloadAndSaveImages rootUrl tocUrl imageUrls

        let localizedPageContents = localizePageContents imageUrls pageContents

        savePage fileName localizedPageContents

        return imageUrls
        ) topPagesDic 
    return $ (map (getSrcFilePath "") . nub . concat) allImageUrls

localizePageContents :: [Url] -> PageContents -> PageContents
localizePageContents imageUrls pageContents = 
    removeBaseHref .  (localizeSrcUrls ("../" ++ imagesFolder) imageUrls) $ pageContents 

localizeSrcUrls :: FilePath -> [Url] -> PageContents  -> PageContents
localizeSrcUrls targetFolder srcUrls pageContents =
    foldr (\srcUrl contents -> 
        replace ("src=\"" ++ srcUrl) ("src=\"" ++ (getSrcFilePath targetFolder srcUrl)) contents) 
        pageContents srcUrls

removeBaseHref :: PageContents -> PageContents
removeBaseHref = unlines . filter (not . containsBaseHref) . lines
    
prettifyList :: Show a => [a] -> String
prettifyList = foldr (++) "" . map ((++)"\n" . show) 

-- ===================
-- Tests
-- ===================

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
    localizeSrcUrlsTests ++
    removeBaseHrefTests 

runTests = do
    runTestTT tests

