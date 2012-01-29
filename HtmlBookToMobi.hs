import HtmlPages(getHtmlPages, filterOutSections, isTopLink)
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

    pagesDic <- getHtmlPages tocUrl

    putStrLn $ prettifyList pagesDic
    
    createKindleStructure pagesDic    

    where
        createKindleStructure pagesDic = do
            let topPagesDic = filter (isTopLink . fst) pagesDic
            let pages = map fst pagesDic
            let topPages = map fst topPagesDic

            createDirectoryIfMissing False title  
            setCurrentDirectory title

            referencedImages <- downloadPages rootUrl topPagesDic

            -- let referencedImages = []
            putStrLn $ prettifyList topPages 
            let opfString = generateOpf topPages referencedImages title language creator 
            writeFile "book.opf" opfString

            let tocString = generateToc topPages title language creator
            writeFile "toc.ncx" tocString

            setCurrentDirectory ".."

downloadPages rootUrl pagesDic = do
    allImageUrls <- mapM (\(fileName, url) -> do
        putStrLn $ "Downloading: " ++ fileName
        pageContents <- downloadPage url

        let imageUrls = getImages pageContents
        downloadAndSaveImages rootUrl imageUrls

        let localizedPageContents = 
                localizeSrcUrls ("../" ++ imagesFolder) pageContents imageUrls 

        savePage fileName localizedPageContents

        return imageUrls
        ) pagesDic 
    return $ (map (getSrcFilePath "") . nub . concat) allImageUrls

localizeSrcUrls :: FilePath -> PageContents -> [Url] -> PageContents
localizeSrcUrls targetFolder pageContents srcUrls =
    foldr (\srcUrl contents -> 
        replace ("src=\"" ++ srcUrl) ("src=\"" ++ (getSrcFilePath targetFolder srcUrl)) contents) 
        pageContents srcUrls

prettifyList :: Show a => [a] -> String
prettifyList = foldr (++) "" . map ((++)"\n" . show) 

-- ===================
-- Tests
-- ===================

localizeSrcUrlsTests =
    [ assertEqual "localizing src urls"
        (localizeSrcUrls filePath pageContents imageUrls) localizedPageContents 
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


tests = TestList $ map TestCase $
    localizeSrcUrlsTests 

runTests = do
    runTestTT tests

