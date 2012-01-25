import HtmlPages(getHtmlPages)
import Constants
import System.FilePath(dropExtension)

title = "Real World Haskell"
language = "en-us"
creator = "Bryan O'Sullivan, Don Stewart, and John Goerzen"

main = do 
    let url = "http://book.realworldhaskell.org/read/"
    dic <- getHtmlPages url

    putStrLn $ generateOpf pagesFolder dic title language creator 


generateOpf pagesFolder dic title language creator = unlines $
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"] ++
    ["<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\">"] ++
        [generateMetaData 1 title language creator] ++
        [generateManifest 1 pagesFolder dic] ++
        [generateSpine    1 dic] ++
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

generateManifest indent pagesFolder dic = unlines $ 
    [(getTabs indent) ++ "<manifest>"] ++ 
       (generateItems pagesFolder dic) ++ 
    [(getTabs indent) ++ "</manifest>"]
    where 
        generateItems pagesFolder dic = 
            map (\(fileName, _) -> generateItem pagesFolder fileName) dic
        generateItem folder fileName =
            getTabs (indent + 1) ++
            "<item id=\"" ++ itemName ++ "\" " ++ 
            "media-type=\"application/xhtml+xml\" href=\"" ++ 
            folder ++ "/" ++ fileName ++"\" />"
            where itemName = dropExtension fileName

generateSpine indent dic = unlines $
    [(getTabs indent) ++ "<spine toc=\"ncx-toc\">"] ++ 
       (map generateItemRef dic) ++ 
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
 

getTabs indent = replicate (indent * 2) ' '
