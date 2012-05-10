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
                    setFormatter lh (simpleLogFormatter "$time $loggername $prio $tid   \t$msg")
                updateGlobalLogger loggerName (addHandler h)
    where prio = stringToPriority p
          stringToPriority = read . map toUpper :: String -> Priority

logd ::  String -> IO ()
logd = debugM loggerName

logi ::  String -> IO ()
logi = infoM loggerName . surroundWithColor infoColor

logw ::  String -> IO ()
logw = warningM loggerName . surroundWithColor warningColor

loge ::  String -> IO ()
loge = errorM loggerName . surroundWithColor errorColor

logt ::  String -> IO ()
logt msg = when tracing (logd msg)

surroundWithColor color msg = "\ESC[" ++ show color ++ "m" ++ msg ++ "\ESC[0m"
