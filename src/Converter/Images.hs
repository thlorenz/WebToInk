module Converter.Images(getImages) where

import Import

import Converter.Types

import Text.HTML.TagSoup(parseTags, Tag(..), (~==))
import System.FilePath(takeExtension)
import Data.List(nub, find)
import Data.Maybe(fromJust)
import Test.HUnit

getImages :: PageContents -> [Url]
getImages = nub . getUrls . filterImages . parseTags
    where 
        getUrls = map getUrl

        getUrl (TagOpen tag pairs) = extractImgSrcUrl pairs
            where 
                extractImgSrcUrl = dropWhile (==' ') . snd . fromJust . findSrcPair
                findSrcPair = find (\(name, url) -> name == "src")

filterImages ::  [Tag String] -> [Tag String]
filterImages = filter (~== imgSrc) 
    where imgSrc = "<img src>" :: String

-- ===================
-- Tests
-- ===================

getImagesTests = TestList $ map TestCase
    [ assertEqual "extracting images when one is contained"
        ["/support/figs/tip.png"] (getImages pageContentsWithOneImage) 
    , assertEqual "extracting images when two are contained"
           ["/support/figs/tip.png", "/support/figs/other.png"]
           (getImages pageContentsWithTwoImages)
    , assertEqual "extracting images when none is contained"
        []  (getImages pageContentsWithoutImage) 
    ]
    where 
        pageContentsWithOneImage  ="<img alt=\"[Tip]\" src=\"/support/figs/tip.png\">"
        pageContentsWithTwoImages = "<img alt=\"[Tip]\" src=\"/support/figs/tip.png\">" ++
                                    "<img alt=\"[Oth]\" src=\"/support/figs/other.png\">" 
        pageContentsWithoutImage  ="<span>see no image</span>"

tests = getImagesTests 
runTests = runTestTT tests
