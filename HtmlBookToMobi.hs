import HtmlPages(getHtmlPages)
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

    createDirectoryIfMissing False title  

    setCurrentDirectory title

    referencedImages <- downloadPages rootUrl pagesDic

    let opfString = generateOpf pagesDic referencedImages title language creator 
    writeFile "book.opf" opfString

    let tocString = generateToc (map fst pagesDic) title language creator
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

