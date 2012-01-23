import HtmlPages(getHtmlPages)
import System.FilePath(dropExtension)

pagesFolder = "pages"
title = "Real World Haskell"
language = "en-us"
creator = "Bryan O'Sullivan, Don Stewart, and John Goerzen"

main = do 
    let url = "http://book.realworldhaskell.org/read/"
    dic <- getHtmlPages url

    putStrLn $ generateOpfXml pagesFolder dic title language creator 


generateOpfXml pagesFolder dic title language creator = unlines $
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"] ++
    ["<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\">"] ++
        [generateMetaData 1 title language creator] ++
        [generateManifest 1 pagesFolder dic] ++
    ["</package>"]

generateMetaData indent title language creator =
    unlines $ map ((getTabs indent)++) $
        ["<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\"" ++
          "xmlns:opf=\"http://www.idpf.org/2007/opf\">"] ++
        map ((getTabs $ indent + 1)++) 
           (["<dc:title>" ++ title ++ "</dc:title>"] ++
            ["<dc:language>" ++ language ++ "</dc:language>"] ++
            ["<dc:creator>" ++ creator ++ "</dc:creator>"] ++
            ["<meta name=\"cover\" content=\"cover\"/>"]) ++
        ["</metadata>"]

generateManifest indent pagesFolder dic = 
    unlines $ [(getTabs indent) ++ "<manifest>"] ++ 
              (generateItems pagesFolder dic) ++ 
              [(getTabs indent) ++ "</manifest>"]
    where 
        generateItems pagesFolder dic = 
            map (\(fileName, _) -> xmlForItem pagesFolder fileName) dic
        xmlForItem folder fileName =
            getTabs (indent + 1) ++
            "<item id=\"" ++ itemName ++ "\" " ++ 
            "media-type=\"application/xhtml+xml\" href=\"" ++ 
            folder ++ "/" ++ fileName ++"\" />"
            where itemName = dropExtension fileName

getTabs indent = replicate (indent * 2) ' '
