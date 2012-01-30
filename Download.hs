module Download
    ( downloadAndSaveImages
    , downloadPage
    , savePage
    , getSrcFilePath
    ) where

import Types
import Constants(pagesFolder, imagesFolder)
import Utils(openUrl)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory, doesFileExist)
import System.IO(hPutStr, withFile, IOMode(..))
import System.FilePath(takeFileName)
import Data.List(isPrefixOf)

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Exception as X

import Test.HUnit

downloadAndSaveImages ::  Url -> Url -> [Url] -> IO [Either a b]
downloadAndSaveImages rootUrl tocUrl imageUrls = do
    createDirectoryIfMissing False imagesFolder
    mapM (downloadAndSaveImage imagesFolder tocUrl rootUrl) imageUrls

downloadPage ::  Url -> IO String
downloadPage url = do
    pageContents <- openUrl url
    return pageContents

savePage ::  FilePath -> String -> IO ()
savePage fileName pageContents = do
    createDirectoryIfMissing False pagesFolder
    write (pagesFolder ++ "/" ++ fileName) pageContents 
    where write fileName pageContents = do 
            withFile fileName WriteMode (\handle -> hPutStr handle pageContents)

downloadAndSaveImage :: FilePath -> Url -> Url -> Url -> IO (Either a b)
downloadAndSaveImage targetFolder rootUrl tocUrl url = do
    let fullRootUrl = resolveUrl rootUrl url
    let fullTocUrl = resolveUrl tocUrl url
    let fullPath = getSrcFilePath targetFolder url

    imageWasDownloadedBefore <- doesFileExist fullPath 
    if imageWasDownloadedBefore 
        then return $ Right undefined 
        else do 
            -- Try to find image at root and then at toc location otherwise we are out of luck
            bytesFromFullPath <- tryDownloadAndSave fullPath fullRootUrl
            case bytesFromFullPath of
                (Left _)  -> tryDownloadAndSave fullPath fullTocUrl
                right     -> return right  


    where tryDownloadAndSave fullPath url = do 
                byteString <- downloadByteString url
                case byteString of 
                    Nothing      -> return $ Left undefined 
                    (Just bytes) -> do { L.writeFile fullPath bytes; return $ Right undefined }


resolveUrl :: Url -> Url -> Url
resolveUrl rootUrl url
        | "http://" `isPrefixOf` url = url
        | otherwise                  = rootUrl ++ "/" ++ url

getSrcFilePath :: FilePath -> Url -> FilePath
getSrcFilePath targetFolder url = targetFolder ++ "/" ++ (takeFileName url)

downloadByteString :: Url -> IO (Maybe L.ByteString)
downloadByteString url = do
    byteString <- try (simpleHttp url)
    case byteString of
        Right x                                   -> return (Just x)
        Left (StatusCodeException status headers) ->
            putStrLn ("An error occured while trying to download: " ++ url)
            >> (putStrLn $ show status)
            >> (return Nothing)

-----------------------
-- ----  Tests  ---- --
-----------------------

resolveUrlTests = 
    [ assertEqual "resolving relative url appends it to root url"
        (resolveUrl root relativeUrl) (root ++ "/" ++ relativeUrl)
    , assertEqual "resolving absolute url returns it as is"
        (resolveUrl root absoluteUrl) (absoluteUrl) 
    ]
    where 
        root = "http://my.root.url"
        relativeUrl = "relative/to/root/some.png"
        absoluteUrl = "http://some.absolute.com"

getSrcFilePathTests = 
    [ assertEqual "getting file path for valid image url"
        (getSrcFilePath targetFolder imgUrl) (targetFolder ++ "/" ++ imgFileName)
    ]
    where
        targetFolder = "someFolder"
        imgFileName = "some.png"
        imgUrl = "/images/" ++ imgFileName
        
tests = TestList $ map TestCase $
    resolveUrlTests ++ 
    getSrcFilePathTests 

runTests = do
    runTestTT tests
