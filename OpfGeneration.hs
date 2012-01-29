module OpfGeneration(generateOpf) where

import Constants
import System.FilePath(dropExtension)
import Utils(getTabs)

generateOpf pagesDic images title language creator = unlines $
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"] ++
    ["<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\">"] ++
        [generateMetaData 1 title language creator] ++
        [generateManifest 1 (map fst pagesDic) images] ++
        [generateSpine    1 pagesDic] ++
        [generateGuide    1 pagesFolder tocPage] ++
    ["</package>"]

generateMetaData indent title language creator = unlines $ 
    map ((getTabs indent)++)
        (["<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\"" ++
          "xmlns:opf=\"http://www.idpf.org/2007/opf\">"] ++
         map ((getTabs $ indent + 1)++) 
           (["<dc:title>" ++ title ++ "</dc:title>"] ++
            ["<dc:language>" ++ language ++ "</dc:language>"] ++
            ["<dc:creator>" ++ creator ++ "</dc:creator>"] ++
            ["<meta name=\"cover\" content=\"cover\"/>"]) ++
         ["</metadata>"])

generateManifest ::  Int -> [FilePath] -> [FilePath] -> String
generateManifest indent pages images = unlines $ 
    [(getTabs indent) ++ "<manifest>"] ++ 
       (generateItems pages) ++ 
       (generateImages images) ++ 
       ["\n" ++ getTabs (indent + 1) ++
        "<item id=\"ncx-toc\" media-type=\"application/x-dtbncx+xml\" href=\"toc.ncx\"/>"] ++
    [(getTabs indent) ++ "</manifest>"]
    where 
        generateItems = map generateItem
        generateItem fileName =
            getTabs (indent + 1) ++
            "<item id=\"" ++ itemName ++ "\" " ++ 
            "media-type=\"application/xhtml+xml\" href=\"" ++ 
            pagesFolder ++ "/" ++ fileName ++"\" />"
            where itemName = dropExtension fileName

        generateImages = map generateImage
        generateImage fileName = 
            getTabs (indent + 1) ++
            "<item media-type=\"image/png\" href=\"" ++ imagesFolder ++ "/" ++ fileName ++ "\"/>"

generateSpine indent pagesDic = unlines $
    [(getTabs indent) ++ "<spine toc=\"ncx-toc\">"] ++ 
       (map generateItemRef pagesDic) ++ 
    [(getTabs indent) ++ "</spine>"]
    where
        generateItemRef (fileName, _) =
            getTabs (indent + 1) ++
            "<itemref idref=\"" ++ itemName ++ "\"/>"
            where itemName = dropExtension fileName

generateGuide indent pagesFolder tocPage = unlines $
    [(getTabs indent) ++ "<guide>"] ++ 
     map ((getTabs $ indent + 1)++) 
        (["<reference type=\"toc\" title=\"Table of Contents\" href=\"" ++ 
         pagesFolder ++ "/" ++ tocPage ++ "\"/>"]) ++
    [(getTabs indent) ++ "</guide>"]
 

