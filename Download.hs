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
import System.FilePath(takeFileName, takeDirectory)
import Data.List(isPrefixOf)

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Exception as X

import Test.HUnit

downloadAndSaveImages ::  Url -> Url -> [Url] -> IO [()]
downloadAndSaveImages rootUrl pageUrl imageUrls = do
    createDirectoryIfMissing False imagesFolder
    mapM (downloadAndSaveImage imagesFolder rootUrl pageUrl) imageUrls

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

downloadAndSaveImage :: FilePath -> Url -> Url -> Url -> IO ()
downloadAndSaveImage targetFolder rootUrl pageUrl url = do
    let fullUrl = resolveUrl rootUrl pageUrl url
    let fullPath = getSrcFilePath targetFolder url

    imageWasDownloadedBefore <- doesFileExist fullPath 
    if imageWasDownloadedBefore 
            then return undefined
            else do 
                byteString <- downloadByteString fullUrl
                case byteString of 
                    Nothing      -> return () 
                    (Just bytes) -> L.writeFile fullPath bytes


resolveUrl :: Url -> Url -> Url -> Url
resolveUrl rootUrl pageUrl url
        | "http://" `isPrefixOf` url = url
        | "/"       `isPrefixOf` url = rootUrl ++ url
        | otherwise                  = pageFolder ++ "/" ++ url
        where pageFolder = takeDirectory pageUrl

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
    [ assertEqual "resolving relative to page url appends it to page url"
        resolvedToPageUrl (resolveUrl root page relativeToPageUrl) 
    , assertEqual "resolving relative to root url appends it to root url"
        resolvedToRootUrl (resolveUrl root page relativeToRootUrl) 
    , assertEqual "resolving absolute url returns it as is"
        (resolveUrl root page absoluteUrl) (absoluteUrl) 
    ]
    where 
        root = "http://my.root.url"
        relativeToRootUrl = "/a/b/some.png" -- / means root 
        resolvedToRootUrl = root ++ relativeToRootUrl

        page = "http://my.root.url/pages/page.html"
        relativeToPageUrl = "a/b/some.png"
        resolvedToPageUrl = "http://my.root.url/pages/a/b/some.png"

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
