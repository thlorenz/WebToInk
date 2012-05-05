{-# LANGUAGE PackageImports #-}
import "WebToInk" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import WebToInk.Converter.Utils

main :: IO ()
main = do
    initLogger "debug"
    logi "Starting WebToInk in development mode ..."
    (port, app) <- getApplicationDev
    forkIO $ runSettings defaultSettings
        { settingsPort = port
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
