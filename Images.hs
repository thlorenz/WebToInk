module Images(getImages) where

import Types
import Text.HTML.TagSoup(parseTags, Tag(..), (~==))
import System.FilePath(takeExtension)
import Data.List(nub)
import Test.HUnit

getImages :: PageContents -> [RelativeUrl]
getImages = getUrls . filterImages . parseTags
    where 
        getUrls = map (snd . getUrl)
        getUrl (TagOpen tag urls) = urls !! 1 -- identify by url id

filterImages ::  [Tag String] -> [Tag String]
filterImages = filter (~== "<img src>") 


-- ===================
-- Tests
-- ===================

tests = TestList $ map TestCase
    [ assertEqual "extracting images when one is contained"
        (getImages pageContentsWithOneImage)
        ["/support/figs/tip.png"]
    , assertEqual "extracting images when none is contained"
        (getImages pageContentsWithoutImage)
        [] 
    ]
    where 
        pageContentsWithOneImage ="<img alt=\"[Tip]\" src=\"/support/figs/tip.png\""
        pageContentsWithoutImage ="<span>see no image</span>"


runTests = do
    runTestTT tests
