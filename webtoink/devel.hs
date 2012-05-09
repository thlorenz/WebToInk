{-# LANGUAGE PackageImports #-}
import "WebToInk" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO, threadDelay)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Starting WebToInk in development mode ..."
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
