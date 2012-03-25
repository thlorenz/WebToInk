module WebToInk.Converter.TocGeneration(generateToc) where

import Import

import WebToInk.Converter.Constants
import WebToInk.Converter.Utils(getTabs)

import System.FilePath(dropExtension)
import Data.Char(toUpper)

import Test.HUnit

-- this assumes that the first page in pages is the "toc.html" (table of contents)
generateToc pages title language author = unlines $
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"] ++
    ["<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">"] ++
    ["<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\" xml:lang=\"" ++ language ++ "\">"] ++
        [generateHead              1 ] ++
        [generateDocTitleAndAuthor 1 title author] ++
        [generateNavMap            1 pages] ++
    ["</ncx>"] 

generateHead indent = unlines $
    map ((getTabs indent)++)
      (["<head>"] ++
       map ((getTabs $ indent + 1)++)
        (["<meta name=\"dtb:uid\" content=\"BookId\"/>"] ++
         ["<meta name=\"dtb:depth\" content=\"2\"/>"] ++
         ["<meta name=\"dtb:totalPageCount\" content=\"0\"/>"] ++
         ["<meta name=\"dtb:maxPageNumber\" content=\"0\"/>"])++
      ["</head>"])

generateDocTitleAndAuthor indent title author = unlines $
    map ((getTabs indent)++)
       (["<docTitle><text>" ++ title ++ "</text></docTitle>"] ++
        ["<docAuthor><text>" ++ author ++ "</text></docAuthor>"])

generateNavMap :: Int -> [FilePath] -> String
generateNavMap indent (toc:chapters) =  unlines $
    map ((getTabs indent)++)
        (["<navMap>"] ++
            [generateNavPoint "toc" toc] ++
            map (generateNavPoint "chapter") chapters ++
        ["</navMap>"])
    where
        generateNavPoint clazz page = unlines $
           map ((getTabs $ indent + 1)++)
            (["<navPoint class=\"" ++ clazz ++ "\" id=\"" ++ itemName ++ "\">"] ++
            (map ((getTabs $ indent + 2)++)
            (["<navLabel>"] ++
             [(getTabs $ indent) ++ "<text>" ++ (makeTitle itemName) ++ "</text>"] ++ 
             ["</navLabel>"] ++
             ["<content src=\"" ++ pagesFolder ++ "/" ++ page ++ "\"/>"])) ++
             [(getTabs $ indent) ++ "</navPoint>"])
            where itemName = dropExtension page
                  makeTitle "toc"  = "Table of Contents"
                  makeTitle chapter = capitalizeWords . map dashToSpace $ chapter

capitalizeWords = unwords . map capitalize . words
    where capitalize (x:xs) = toUpper x : xs

dashToSpace '-' = ' '
dashToSpace c   = c
           
-----------------------
-- ----  Tests  ---- --
-----------------------

debug = do
    let pages = ["toc.html", "this-is-the-title.html"]
    putStrLn $ generateToc pages "title" "en-us" "Some Author"
