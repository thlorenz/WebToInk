module Download(downloadPages, downloadImages) where

import Types
import Constants(pagesFolder, imagesFolder)
import Utils(openUrl)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory, doesFileExist)
import System.IO(hPutStr, withFile, IOMode(..))
import System.FilePath(takeFileName)
import Data.List(isPrefixOf)

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

import Test.HUnit

downloadImages rootUrl imageUrls = do
    createDirectoryIfMissing False imagesFolder
    mapM (downloadImage imagesFolder rootUrl) imageUrls

downloadPages ::  [(FilePath, String)] -> IO ()
downloadPages dic = do
    createDirectoryIfMissing False pagesFolder
    setCurrentDirectory ".."
    setCurrentDirectory pagesFolder 
    mapM downloadPage dic
    setCurrentDirectory ".."

downloadPage ::  (FilePath, String) -> IO ()
downloadPage (fileName, url) = do
    pageContents <- openUrl url
    write fileName pageContents 
    where write fileName pageContents = do 
            withFile fileName WriteMode (\handle -> hPutStr handle pageContents)

downloadImage :: FilePath -> Url -> Url -> IO ()
downloadImage targetFolder rootUrl url = do
    let fullUrl = resolveUrl rootUrl url
    let fullPath = getImageFilePath targetFolder url
    imageWasDownloadedBefore <- doesFileExist fullPath
    if imageWasDownloadedBefore 
        then return undefined
        else simpleHttp fullUrl >>= L.writeFile fullPath

resolveUrl :: Url -> Url -> Url
resolveUrl rootUrl url
        | "http://" `isPrefixOf` url = url
        | otherwise                  = rootUrl ++ "/" ++ url

getImageFilePath :: FilePath -> Url -> FilePath
getImageFilePath targetFolder url = targetFolder ++ "/" ++ (takeFileName url)

-- ===================
-- Tests
-- ===================

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

getImageFilePathTests = 
    [ assertEqual "getting file path for valid image url"
        (getImageFilePath targetFolder imgUrl) (targetFolder ++ "/" ++ imgFileName)
    ]
    where
        targetFolder = "someFolder"
        imgFileName = "some.png"
        imgUrl = "/images/" ++ imgFileName
        
tests = TestList $ map TestCase $
    resolveUrlTests ++ 
    getImageFilePathTests 

runTests = do
    runTestTT tests
