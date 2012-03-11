module Converter.Download
    ( downloadAndSaveImages
    , downloadPage
    , savePage
    , getSrcFilePath
    ) where

import Converter.Types
import Converter.Constants(pagesFolder, imagesFolder)
import Converter.Utils (openUrl, downloadByteString)

import System.Directory(createDirectoryIfMissing, setCurrentDirectory, doesFileExist)
import System.IO(hPutStr, withFile, IOMode(..))
import System.FilePath(takeFileName, takeDirectory)
import Data.List(isPrefixOf)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Exception as X

import Test.HUnit

downloadAndSaveImages ::  Url -> Url -> [Url] -> IO [()]
downloadAndSaveImages rootUrl pageUrl imageUrls = do
    createDirectoryIfMissing False imagesFolder
    mapM (downloadAndSaveImage imagesFolder rootUrl pageUrl) imageUrls

downloadPage ::  Url -> IO (Maybe String)
downloadPage = openUrl . cleanUrl

savePage ::  FilePath -> String -> IO ()
savePage fileName pageContents = do
    createDirectoryIfMissing False pagesFolder
    writeFile (pagesFolder ++ "/" ++ fileName) pageContents 

downloadAndSaveImage :: FilePath -> Url -> Url -> Url -> IO ()
downloadAndSaveImage targetFolder rootUrl pageUrl url = do
    let fullUrl = resolveUrl rootUrl pageUrl url
    let fullPath = getSrcFilePath targetFolder url

    putStrLn $ "Downloading image at " ++ fullUrl

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
        | "http://"  `isPrefixOf` url = cleanedUrl
        | "https://" `isPrefixOf` url = cleanedUrl
        | "/"        `isPrefixOf` url = rootUrl ++ cleanedUrl
        | otherwise                  = pageFolder ++ "/" ++ cleanedUrl
        where pageFolder = takeDirectory pageUrl
              cleanedUrl = cleanUrl url

cleanUrl = takeWhile (\x -> x /= '?' && x /= '#')

getSrcFilePath :: FilePath -> Url -> FilePath
getSrcFilePath targetFolder url = targetFolder ++ "/" ++ takeFileName url

-----------------------
-- ----  Tests  ---- --
-----------------------

resolveUrlTests = 
    [ assertEqual "resolving relative to page url appends it to page url"
        resolvedToPageUrl (resolveUrl root page relativeToPageUrl) 
    , assertEqual "resolving relative to root url appends it to root url"
        resolvedToRootUrl (resolveUrl root page relativeToRootUrl) 
    , assertEqual "resolving absolute url returns it as is"
        absoluteUrl (resolveUrl root page absoluteUrl)
    , assertEqual "resolving image url containing ?" 
       "http://some/image.png" (resolveUrl root page "http://some/image.png?query")
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

runTests = runTestTT tests
