module WebToInk.Converter.CommandLineParser(Args(..), legend, parseArgs) where

import Import 

import WebToInk.Converter.Types

import System.Environment (getArgs)
import Test.HUnit
import Data.Maybe

data Args  = Args   { argsTitle           :: Maybe String
                    , argsLanguage        :: String
                    , argsAuthor          :: Maybe String
                    , argsTocUrl          :: Maybe Url
                    , argsFolder          :: FilePath
                    } deriving (Show, Eq)

titleOpt    = "--title"
languageOpt = "--language"
authorOpt   = "--author"
tocOpt      = "--toc"
folderOpt   = "--folder"

options = [ titleOpt, languageOpt, authorOpt, tocOpt ]

legend = 
     show (titleOpt    ,  "-t"  ,  "Book Title") ++ "\n" ++
     show (languageOpt ,  "-l"  ,  "Language (default en-US)") ++ "\n" ++
     show (authorOpt   ,  "-a"  ,  "Book author") ++ "\n" ++
     show (tocOpt      ,  "-c"  ,  "Url to the page that contains the table of contents of the book") ++ "\n" ++
     show (folderOpt   ,  "-f"  ,  "Target argsFolder in which to create book argsFolder to store downloaded pages,  images and generated mobi (default is '.')")

parseArgs :: [String] -> Args
parseArgs options = Args { argsTitle    = tryGetArg titleOpt
                         , argsLanguage = getArg languageOpt "en-us"
                         , argsAuthor   = tryGetArg authorOpt       
                         , argsTocUrl   = tryGetArg tocOpt
                         , argsFolder   = getArg folderOpt "."  
                         }
    where 
        normOpts = normalizeOptions options
        tryGetArg option = (extractArg . dropWhile (/= option)) normOpts 
            where extractArg (x1:x2:xs) = Just x2
                  extractArg _          = Nothing

        getArg option alternative = fromMaybe alternative (tryGetArg option)

normalizeOptions :: [String] -> [String]
normalizeOptions = map normalizeOption

normalizeOption :: String -> String
normalizeOption "-t" = titleOpt
normalizeOption "-l" = languageOpt
normalizeOption "-a" = authorOpt
normalizeOption "-c" = tocOpt
normalizeOption "-f" = folderOpt
normalizeOption x    = x

-----------------------
-- ----  Tests  ---- --
-----------------------

normalizeOptionsTests =
    [ assertEqual "empty args" [] $ normalizeOptions [] 
    , assertEqual "no shortcuts" allVerbose $ normalizeOptions allVerbose 
    , assertEqual "title shortcut" ["--title", "some"] $ normalizeOptions ["-t", "some"] 
    , assertEqual "title and url shortcuts" 
        ["--title", "some", "--toc", "http"] $ normalizeOptions ["-t", "some", "-c", "http"] 
    , assertEqual "title and url shortcuts and verbose options" 
        ["--title", "some", "--author ", "verbose", "--toc", "http"] $ 
        normalizeOptions ["-t", "some", "--author ", "verbose", "-c", "http"] 
    ]
    where allVerbose = ["--title", "some", "--toc", "http"]

parseArgsTests =
    [ assertEqual "no args" 
        (Args Nothing defLang  Nothing  Nothing defFolder) $ parseArgs []
    , assertEqual "title given"
        (Args (Just givenTitle) defLang  Nothing Nothing defFolder) $
        parseArgs ["--title", givenTitle]
    , assertEqual "title and argsAuthor given"
        (Args (Just givenTitle) defLang  (Just givenAuthor) Nothing defFolder) $
        parseArgs ["--title", givenTitle, "-a", givenAuthor]
    , assertEqual "title, argsLanguage and argsAuthor given"
        (Args (Just givenTitle) givenLanguage (Just givenAuthor) Nothing givenFolder) $
        parseArgs ["--title", givenTitle, "-a", givenAuthor, "-l", givenLanguage, "-f", givenFolder]
    ]

    where defLang = "en-us"
          defFolder = "."
          givenTitle = "the title"
          givenAuthor = "the author"
          givenLanguage = "de-DE"
          givenFolder = "../mybooks"

tests = TestList $ map TestCase $
    normalizeOptionsTests ++
    parseArgsTests 

runTests = runTestTT tests
