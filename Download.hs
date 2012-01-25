module Download(downloadPages) where

import Types
import Constants(pagesFolder, imagesFolder)
import Utils(openUrl)
import System.Directory(createDirectoryIfMissing, setCurrentDirectory)
import System.IO(hPutStr, withFile, IOMode(..))
import Data.List(isPrefixOf)

import Test.HUnit

downLoadImages rootUrl imageUrls = do
    createDirectoryIfMissing False imagesFolder
    setCurrentDirectory imagesFolder
    setCurrentDirectory ".."

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

resolveUrl :: Url -> Url -> Url
resolveUrl rootUrl url
        | "http://" `isPrefixOf` url = url
        | otherwise                  = rootUrl ++ "/" ++ url

-- ===================
-- Tests
-- ===================

resolveUrlTests = TestList $ map TestCase   
    [ assertEqual "resolving relative url appends it to root url"
        (resolveUrl root relativeUrl) (root ++ "/" ++ relativeUrl)
    , assertEqual "resolving absolute url returns it as is"
        (resolveUrl root absoluteUrl) (absoluteUrl) 
    ]
    where 
        root = "http://my.root.url"
        relativeUrl = "relative/to/root/some.png"
        absoluteUrl = "http://some.absolute.com"

tests = resolveUrlTests 
runTests = do
    runTestTT tests
