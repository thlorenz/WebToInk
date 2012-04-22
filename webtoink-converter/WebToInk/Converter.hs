module WebToInk.Converter where

import System.Environment (getArgs)

import WebToInk.Converter.CommandLineParser (Args(..), legend, parseArgs)
import WebToInk.Converter.ConverterService (prepareKindleGeneration)

main :: IO ()
main = do   

    args <- (fmap parseArgs) getArgs 

    case argsTocUrl args of
        Just tocUrl -> prepareKindleGeneration 
                            (argsTitle args)     
                            (argsAuthor args)   
                            (argsLanguage args) 
                            (tocUrl) 
                            -- (argsFolder args)
                            "../books"
                       >> return ()
        Nothing     -> putStrLn legend
                          

