module Converter where

import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import Control.Monad (forM)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf, nub)
import System.Environment (getArgs)

import Test.HUnit

import Converter.HtmlPages 
import Converter.Images (getImages)
import Converter.Download (downloadPage, savePage, downloadAndSaveImages, getSrcFilePath)
import Converter.OpfGeneration (generateOpf)
import Converter.TocGeneration (generateToc)
import Converter.CommandLineParser (Args(..), legend, parseArgs)
import Converter.Types
import Converter.Constants

main = do   

    args <- (fmap parseArgs) getArgs 

    case argsTocUrl args of
        Just tocUrl -> prepareKindleGeneration 
                            (argsTitle args)     
                            (argsAuthor args)   
                            (argsLanguage args) 
                            (tocUrl) 
                            (argsFolder args)
        Nothing     -> putStrLn legend
                          

prepareKindleGeneration :: Maybe String -> Maybe String -> String -> Url -> FilePath -> IO ()
prepareKindleGeneration maybeTitle maybeAuthor language tocUrl folder = do

    maybeGetHtmlPagesResult <- getHtmlPages tocUrl

    case maybeGetHtmlPagesResult of
        Just result   -> prepare result 
        Nothing       -> putStrLn "Error could not download table of contents and processed no html pages!!!"
  where 
        prepare (GetHtmlPagesResult tocContent pagesDic) = do
            let author = resolveAuthor maybeAuthor tocContent
            let title = resolveTitle maybeTitle tocContent

            let topPagesDic = filter (isTopLink . fst) pagesDic
            let topPages = map fst topPagesDic

            putStrLn $ prettifyList topPagesDic
            
            createKindleStructure title author topPagesDic topPages

          where 
            createKindleStructure title author topPagesDic topPages = do
                let targetFolder = folder ++ "/" ++ title
                 
                createDirectoryIfMissing False targetFolder  
                setCurrentDirectory targetFolder

                result <- downloadPages tocUrl topPagesDic    
                
                let failedFileNames = map fileName $ failedPages result
                let goodTopPages = filter (`notElem` failedFileNames) topPages

                putStrLn "\nDownload Summary"
                putStrLn   "----------------\n"

                putStr "Successfully downloaded:"
                putStrLn $ (prettifyList goodTopPages) ++ "\n"

                putStr "Failed to download:"
                putStrLn $ (prettifyList failedFileNames) ++"\n"

                putStrLn "Generating book.opf"
                let opfString = generateOpf goodTopPages (allImageUrls result) title language author 
                writeFile "book.opf" opfString

                putStrLn "Generating toc.ncx"
                let tocString = generateToc goodTopPages title language author
                writeFile "toc.ncx" tocString

                setCurrentDirectory ".."
                    

downloadPages :: Url -> [(FilePath, Url)] -> IO DownloadPagesResult 
downloadPages tocUrl topPagesDic = do
    let rootUrl = getRootUrl tocUrl

    downloadResults <- mapM (\(fileName, pageUrl) ->
        tryProcessPage $ PageInfo rootUrl pageUrl fileName) topPagesDic 
    
    let uniqueImageUrls = 
            map (getSrcFilePath "") . nub . concat . map allImageUrls $ downloadResults 
    let allFailedPages = concat . map failedPages $ downloadResults
    return $ DownloadPagesResult uniqueImageUrls allFailedPages

tryProcessPage :: PageInfo -> IO (DownloadPagesResult)
tryProcessPage pi = do
    maybePageContents <- downloadPage (pageUrl pi)

    case maybePageContents of
        Just pageContents -> do
            imageUrls <- processPage pi pageContents 
            return $ DownloadPagesResult imageUrls []
        Nothing           -> return $ DownloadPagesResult [] [pi]
        
processPage :: PageInfo -> PageContents -> IO [String]
processPage pi pageContents = do
    let imageUrls = (filter (not . ("https:" `isPrefixOf`)) . getImages) pageContents

    downloadAndSaveImages (rootUrl pi) (pageUrl pi) imageUrls

    let localizedPageContents = localizePageContents imageUrls pageContents

    savePage (fileName pi) localizedPageContents

    return imageUrls

localizePageContents :: [Url] -> PageContents -> PageContents
localizePageContents imageUrls pageContents = 
    removeBaseHref .  localizeSrcUrls ("../" ++ imagesFolder) imageUrls $ pageContents 

localizeSrcUrls :: FilePath -> [Url] -> PageContents  -> PageContents
localizeSrcUrls targetFolder srcUrls pageContents =
    foldr (\srcUrl contents -> 
        replace ("src=\"" ++ srcUrl) ("src=\"" ++ getSrcFilePath targetFolder srcUrl) contents) 
        pageContents srcUrls

removeBaseHref :: PageContents -> PageContents
removeBaseHref = unlines . filter (not . containsBaseHref) . lines
    
prettifyList :: Show a => [a] -> String
prettifyList = foldr ((++) . (++) "\n" . show) ""

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

runTests = runTestTT tests

