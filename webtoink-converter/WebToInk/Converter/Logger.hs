module WebToInk.Converter.Logger ( initLogger
                                 , logd
                                 , logi
                                 , logw
                                 , loge
                                 , logt
                                 ) where
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Char (toUpper)
import Control.Monad (when)


tracing = True
loggerName = "Converter"

debugColor   =  80
traceColor   =  1 
errorColor   =  91
infoColor    =  93
warningColor =  94

initLogger ::  String -> Maybe FilePath -> IO ()
initLogger p mbFilePath =
    updateGlobalLogger loggerName (setLevel prio) 
    >>  case mbFilePath of
            Nothing     -> return ()
            Just path   -> do
                h <- fileHandler path DEBUG >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "$time $loggername $tid $msg")
                updateGlobalLogger loggerName (addHandler h)
    where prio = stringToPriority p
          stringToPriority = read . map toUpper :: String -> Priority

logd ::  String -> IO ()
logi ::  String -> IO ()
logw ::  String -> IO ()
loge ::  String -> IO ()
logt ::  String -> IO ()

logd = debugM loggerName   . surroundWithColor debugColor   . ("[DBG] " ++)
logi = infoM loggerName    . surroundWithColor infoColor    . ("[INF] " ++)
logw = warningM loggerName . surroundWithColor warningColor . ("[WRN] " ++)
loge = errorM loggerName   . surroundWithColor errorColor   . ("[ERR] " ++)
logt msg = when tracing (debugM loggerName . surroundWithColor traceColor . ("[TRC] " ++) $ msg)

surroundWithColor color msg = "\ESC[" ++ show color ++ "m" ++ msg ++ "\ESC[0m"

main = do
    go 255
    where 
        go index = 
            if index > 0 
                then do
                    putStr (surroundWithColor index ( show index ++ ", "))
                    go (index - 1)
                else
                    return ()



