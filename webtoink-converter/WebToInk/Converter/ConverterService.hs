module WebToInk.Converter.ConverterService  ( prepareKindleGeneration
                                            , getTitle
                                            , getMobi
                                            ) where

import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)

import System.Cmd (rawSystem)
import System.Exit (ExitCode (..))
import System.Posix.Files (setFileMode, unionFileModes, ownerModes, otherExecuteMode)
import System.FilePath(combine, takeExtension, (<.>))

import Data.Char (isAscii)
import Data.List (isPrefixOf, nub)
import Data.Functor ((<$>))

import Control.Applicative((<*>))
import Control.Exception (throwIO, try, Exception)

import qualified Data.ByteString.Char8 as C

import WebToInk.Converter.HtmlPages 
import WebToInk.Converter.Images (getImages)
import WebToInk.Converter.Download (downloadPage, savePage, downloadAndSaveImages, getSrcFilePath)
import WebToInk.Converter.OpfGeneration (generateOpf)
import WebToInk.Converter.TocGeneration (generateToc)
import WebToInk.Converter.Types
import WebToInk.Converter.Constants
import WebToInk.Converter.Exceptions
import WebToInk.Converter.Utils
import WebToInk.Converter.Logger



-- | Tries to download page at given url and resolve title.
-- If anything goes wrong an empty string is returned.
getTitle :: Url -> IO (Either String String)
getTitle url = do 
    logd $ "Getting title for: " ++ url
    result <- try go :: (Exception a) => IO (Either a String)
    case result of
        Right title     -> return $ Right title
        Left exception  -> handleException exception
  where
    go = do
        maybeToc <- downloadPage url
        logt "Downloaded page, resolving title"
        return $ case maybeToc of
            Just toc -> resolveTitle Nothing toc
            Nothing  -> ""

-- | Resolves page at url and all direct children.
-- Downloads all the pages and their images.
-- Then generates a .mobi file from it using the kindlegen tool
-- Finally it returns the path to the generated mobi file from which it can be downloaded.
getMobi :: Url -> String -> String -> FilePath -> IO (Either String FilePath)
getMobi url title author targetFolder = do
    logd $ "Preparing " ++ title ++ " by " ++ author

    result <- try go :: (Exception a) => IO (Either a FilePath)
    case result of
        Right fullFilePath  -> return $ Right fullFilePath 
        Left  exception     -> handleException exception
  where 
    go = do
        path <- prepareKindleGeneration (Just title) (Just author) "en-us" url targetFolder 
        
        -- Allow all users to enter path and read from it since we want to make this available
        -- TODO: handle the case where current user is not permitted to change permissions
        -- setFileMode path $ unionFileModes ownerModes otherExecuteMode

        let targetFile = filter isAscii title<.>"mobi"
            
        runKindlegen targetFile path True

    runKindlegen targetFile path firstTime = do
        result <- rawSystem "kindlegen" [ "-o", targetFile, combine path "book.opf" ]
        case result of
            ExitSuccess                 -> return (combine path targetFile)
            
            -- In case of warnings (1) we are ok
            ExitFailure 1               -> return (combine path targetFile)
            
            -- In case of problems related to javascript (2) remove it from all pages and try again
            ExitFailure 2               -> if firstTime 
                                               then removeJavaScriptsAndTryAgain targetFile path
                                               else throwIO $ KindlegenException 2
                                               
            -- All others are problematic and need to be raised
            ExitFailure code            -> throwIO $ KindlegenException code

    removeJavaScriptsAndTryAgain targetFile path = do
         
        htmlFiles <- fmap getHtmlFilePaths . getDirectoryContents $ pagesFullPath
        mapM_ removeScriptsFromFileAndSave htmlFiles
        
        runKindlegen targetFile path False

      where 
        removeScriptsFromFileAndSave fullPath = removeScriptsFromFile fullPath >>= saveContentsToFile fullPath

        removeScriptsFromFile = fmap (removeScripts . C.unpack) . C.readFile

        saveContentsToFile fullPath = C.writeFile fullPath . C.pack

        getHtmlFilePaths =  map (combine pagesFullPath) . filter isHtmlFile
        pagesFullPath = combine path pagesFolder
        isHtmlFile file = let extension = takeExtension file
                            in  extension == ".html" || extension == ".htm"


main = testConverter

testLogger = do
    initLogger "debug" (Just "./debug.log")
    logi "hello world"
    logd "hello world"
    logt "hello world"
    loge "hello world"
    logw "hello world"


testConverter = do
    initLogger "debug" (Just "./debug.log")

    result <- getMobi url title author targetFolder
    case result of
        Right filePath     -> logi $ "Success: " ++ filePath
        Left error         -> loge $ "Error: " ++ error 
    return ()
  where 
    url = "http://static.springsource.org/spring/docs/current/spring-framework-reference/html/overview.html"
    title = "Spring"
    author = "Team"
    targetFolder = "../books"
    
prepareKindleGeneration :: Maybe String -> Maybe String -> String -> Url -> FilePath -> IO FilePath 
prepareKindleGeneration maybeTitle maybeAuthor language tocUrl folder = do
    logd $ "Getting pages from: " ++ tocUrl
    maybeGetHtmlPagesResult <- getHtmlPages tocUrl
    case maybeGetHtmlPagesResult of
        Just result   -> logd ("Got pages, creating webtoink temp directory at: " ++ folder)
                         >> createTempDirectory folder "webtoink" >>= prepare result
        Nothing       -> loge "Could not download table of contents and processed no html pages"
                         >> throwIO TableOfContentsCouldNotBeDownloadedException
  where 
    prepare (GetHtmlPagesResult tocContent pagesDic) targetFolder = do
        let author = resolveAuthor maybeAuthor tocContent
        let title = resolveTitle maybeTitle tocContent

        let topPagesDic = filter (isTopLink . fst) pagesDic
        let topPages = map fst topPagesDic

        logd $  "Preparing for kindlegen " ++ "(Author: " ++ show author ++  "Title: " ++ show title ++ ")"
        logt $ prettifyList ", " topPagesDic
        
        createKindleStructure title author topPagesDic topPages targetFolder

      where 
        correctFolder targetFolder (filePath, url) = (combine targetFolder filePath, url)
        createKindleStructure title author topPagesDic topPages targetFolder = do
            logd $ "created temp folder" ++ show targetFolder
             
            logd "Starting to download pages"

            result <- downloadPages tocUrl topPagesDic targetFolder
            
            let failedFileNames = map piFileName $ failedPages result
            let goodTopPages = filter (`notElem` failedFileNames) topPages

            logt $ "Successfully downloaded: " ++ (prettifyList ", " goodTopPages)

            logt $ "Failed to download: " ++ (prettifyList ", " failedFileNames)

            logd "Generating book.opf"
            let opfString = generateOpf goodTopPages (allImageUrls result) title language author 
            writeFile (combine targetFolder "book.opf") opfString

            logd "Generating toc.ncx"
            let tocString = generateToc goodTopPages title language author
            writeFile (combine targetFolder "toc.ncx") tocString

            return targetFolder
                

downloadPages :: Url -> [(FilePath, Url)] -> FilePath -> IO DownloadPagesResult 
downloadPages tocUrl topPagesDic targetFolder = do
    let rootUrl = getRootUrl tocUrl

    downloadResults <- mapM (\(fileName, pageUrl) ->
        tryProcessPage (PageInfo rootUrl pageUrl fileName) targetFolder) topPagesDic 
    
    let uniqueImageUrls = 
            map (getSrcFilePath "") . nub . concatMap allImageUrls $ downloadResults 
    let allFailedPages = concatMap failedPages downloadResults
    return $ DownloadPagesResult uniqueImageUrls allFailedPages

tryProcessPage :: PageInfo -> FilePath -> IO DownloadPagesResult
tryProcessPage pi targetFolder = do
    maybePageContents <- downloadPage (piPageUrl pi)

    case maybePageContents of
        Just pageContents -> do
            imageUrls <- processPage pi pageContents targetFolder
            return $ DownloadPagesResult imageUrls []
        Nothing           -> return $ DownloadPagesResult [] [pi]
        
processPage :: PageInfo -> PageContents -> FilePath -> IO [String]
processPage pi pageContents targetFolder = do
    let imageUrls = (filter (not . ("https:" `isPrefixOf`)) . getImages) pageContents

    downloadAndSaveImages targetFolder (piRootUrl pi) (piPageUrl pi) imageUrls

    let adaptedPageContents = cleanAndLocalize imageUrls pageContents

    savePage targetFolder (piFileName pi) adaptedPageContents

    return imageUrls

cleanAndLocalize :: [Url] -> PageContents -> PageContents
cleanAndLocalize imageUrls pageContents = 
    removeBaseHref .  localizeSrcUrls ("../" ++ imagesFolder) imageUrls $ pageContents 

prettifyList :: Show a => String -> [a]  -> String
prettifyList delim = foldr ((++) . (++) delim . show) ""

handleException exception = do
    let exceptionInfo = getExceptionInfo exception
    loge (fst exceptionInfo) 
    return $ Left (snd exceptionInfo)
  where
    getExceptionInfo exception = 
        case exception of
            TableOfContentsCouldNotBeDownloadedException   -> ( "TableOfContentsCouldNotBeDownloadedException."
                                                              , "Could not download page. Please check the url and/or make sure that the server is available.")
            ex@(KindlegenException code)                   -> ( show ex
                                                              , "The kindlegen tool was unable to convert the page. Please try another format.")
            ex                                             -> ( "Unknown Exception: " ++ show ex
                                                              , "An unexcpected error occured. Please try again later.")


