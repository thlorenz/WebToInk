module TocGeneration(generateToc) where
import Constants
import System.FilePath(dropExtension)
import Utils(getTabs)

generateToc pagesDic title language author = unlines $
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"] ++
    ["<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">"] ++
    ["<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\" xml:lang=\"" ++ language ++ "\">"] ++
        [generateHead             1 ] ++
        [generateDocTitleAndAuthor 1 title author] ++
        [generateNavMap           1 pagesDic] ++
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

generateNavMap = undefined
