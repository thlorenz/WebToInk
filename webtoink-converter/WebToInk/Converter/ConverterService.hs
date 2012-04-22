module WebToInk.Converter.ConverterService  ( prepareKindleGeneration
                                            , getTitle
                                            , getMobi
                                            ) where


import System.Directory (createDirectoryIfMissing)
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)

import System.FilePath (combine)

import System.Cmd (rawSystem)
import System.Posix.Files (setFileMode, unionFileModes, ownerModes, otherExecuteMode)
import System.FilePath(combine, (<.>))

import Data.Char (isAscii)
import Data.List.Utils (replace)
import Data.List (isPrefixOf, nub)

import WebToInk.Converter.HtmlPages 
import WebToInk.Converter.Images (getImages)
import WebToInk.Converter.Download (downloadPage, savePage, downloadAndSaveImages, getSrcFilePath)
import WebToInk.Converter.OpfGeneration (generateOpf)
import WebToInk.Converter.TocGeneration (generateToc)
import WebToInk.Converter.Types
import WebToInk.Converter.Constants

-- | Tries to download page at given url and resolve title.
-- If anything goes wrong an empty string is returned.
getTitle :: Url -> IO String
getTitle url = do 
  maybeToc <- downloadPage url
  return $ case maybeToc of
      Just toc -> resolveTitle Nothing toc
      Nothing  -> ""

-- | Resolves page at url and all direct children.
-- Downloads all the pages and their images.
-- Then generates a .mobi file from it using the kindlegen tool
-- Finally it returns the path to the generated mobi file from which it can be downloaded.
getMobi :: Url -> String -> String -> FilePath -> IO FilePath
getMobi url title author targetFolder = do
    
    -- TODO: wrap all this inside try catch

    putStrLn $ "Preparing " ++ title ++ " by " ++ author
    path <- prepareKindleGeneration (Just title) (Just author) "en-us" url targetFolder

    -- Allow all users to enter path and read from it since we want to make this available
    -- TODO: handle the case where current user is not permitted to change permissions
    setFileMode path $ unionFileModes ownerModes otherExecuteMode

    rawSystem "kindlegen" [ "-o", targetFile, combine path "book.opf" ]

    return $ combine path targetFile

  where targetFile = (filter isAscii title)<.>"mobi"
    
prepareKindleGeneration :: Maybe String -> Maybe String -> String -> Url -> FilePath -> IO FilePath 
prepareKindleGeneration maybeTitle maybeAuthor language tocUrl folder = do

    targetFolder <- createTempDirectory folder "webtoink"
    maybeGetHtmlPagesResult <- getHtmlPages tocUrl

    case maybeGetHtmlPagesResult of
        Just result   -> prepare result targetFolder
        Nothing       -> putStrLn "Error could not download table of contents and processed no html pages!!!"
                         >> return ""
  where 
    prepare (GetHtmlPagesResult tocContent pagesDic) targetFolder = do
        let author = resolveAuthor maybeAuthor tocContent
        let title = resolveTitle maybeTitle tocContent

        let topPagesDic = filter (isTopLink . fst) pagesDic
        let topPages = map fst topPagesDic

        putStrLn $ prettifyList topPagesDic
        
        createKindleStructure title author topPagesDic topPages targetFolder

      where 
        correctFolder targetFolder (filePath, url) = (combine targetFolder filePath, url)
        createKindleStructure title author topPagesDic topPages targetFolder = do
            putStrLn $ "creating temp folder in " ++ (show folder)

            putStrLn $ "created temp folder" ++ (show targetFolder)
             
            putStrLn "Starting to download pages"

            result <- downloadPages tocUrl topPagesDic targetFolder
            
            let failedFileNames = map piFileName $ failedPages result
            let goodTopPages = filter (`notElem` failedFileNames) topPages

            putStrLn "\nDownload Summary"
            putStrLn   "----------------\n"

            putStr "Successfully downloaded:"
            putStrLn $ (prettifyList goodTopPages) ++ "\n"

            putStr "Failed to download:"
            putStrLn $ (prettifyList failedFileNames) ++"\n"

            putStrLn "Generating book.opf"
            let opfString = generateOpf goodTopPages (allImageUrls result) title language author 
            writeFile (combine targetFolder "book.opf") opfString

            putStrLn "Generating toc.ncx"
            let tocString = generateToc goodTopPages title language author
            writeFile (combine targetFolder "toc.ncx") tocString

            return targetFolder
                

downloadPages :: Url -> [(FilePath, Url)] -> FilePath -> IO DownloadPagesResult 
downloadPages tocUrl topPagesDic targetFolder = do
    let rootUrl = getRootUrl tocUrl

    downloadResults <- mapM (\(fileName, pageUrl) ->
        tryProcessPage (PageInfo rootUrl pageUrl fileName) targetFolder) topPagesDic 
    
    let uniqueImageUrls = 
            map (getSrcFilePath "") . nub . concat . map allImageUrls $ downloadResults 
    let allFailedPages = concat . map failedPages $ downloadResults
    return $ DownloadPagesResult uniqueImageUrls allFailedPages

tryProcessPage :: PageInfo -> FilePath -> IO (DownloadPagesResult)
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
    {- removeScripts . -} removeBaseHref .  localizeSrcUrls ("../" ++ imagesFolder) imageUrls $ pageContents 

prettifyList :: Show a => [a] -> String
prettifyList = foldr ((++) . (++) "\n" . show) ""

main = do  -- getMobi url title author targetFolder
    
   let cleanedTitle =  title 
   putStrLn cleanedTitle 
  where 
    url = "http://thorstenlorenz.wordpress.com/2012/03/02/lion-logging-to-growl-messages-from-haskell-using-hslogger-an-growlnotify/"
    title = "Logging to Growl from Haskell running on Lion « Thorsten Lorenz"
    author = "Thorsten Lorenz"
    targetFolder = "../books"
