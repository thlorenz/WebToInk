import Network (listenOn, accept, PortID(..), Socket)
import System (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted " ++ (show host) ++ ":" ++ (show port)
   
    -- no buffering for client's socket handle
    hSetBuffering handle NoBuffering

    forkIO $ commandProcessor handle

    -- handle more incoming connections
    sockHandler sock

commandProcessor :: Handle -> IO () 
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        "echo"  -> echoCommand handle cmd
        "add"   -> addCommand handle cmd
        _       -> hPutStrLn handle "Unknown command"
    
    -- recurse to execute several commands over same connection
    commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
    hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = do
    hPutStrLn handle $ show $ (read $ cmd !! 1) + (read $ cmd !! 2)

