import Network (listenOn, accept, PortID(..), Socket)
import System (getArgs)
import System.IO
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral 3000 -- fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (show port)
    sockHandler sock 0

sockHandler :: Socket -> Int -> IO ()
sockHandler sock sessionId = do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted " ++ (show host) ++ ":" ++ (show port)
   
    -- no buffering for client's socket handle
    hSetBuffering handle NoBuffering

    forkIO $ requestProcessor handle sessionId

    -- handle more incoming connections
    sockHandler sock (sessionId + 1)

requestProcessor :: Handle -> Int -> IO ()
requestProcessor handle sessionId = do

    reqString <- hGetLine handle
    putStrLn $ "client: [" ++ (show $ length reqString) ++ "] " ++ reqString
    
    -- Empty line in request denotes that it is complete
    if (length reqString <= 1 )
        then do
            body <- readFile "public/index.html"
            -- Response Header
            hPutStrLn handle (header body)
            hPutStrLn handle ""

            -- Response Body
            hPutStrLn handle body

            -- Send all and close connection
            hFlush handle
            hClose handle

            print "DONE"
        else 
            requestProcessor handle sessionId

  where 
    status        =  "HTTP/1.1 200 OK"
    server        =  "MyHaskellServer/0.0.1 (Darwin)"
    lastModified  =  "Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT"
    etag          =  "Etag: \"3f80f-1b6-3e1cb03b\""
    acceptRanges  =  "Accept-Ranges: bytes"
    -- length of body + <CR><LF>
    contentLength body =  "Content-Length: " ++ (show $ (length body + 2))
    connection    =  "Connection: close"
    contentType   =  "Content-Type: text/html; charset=UTF-8"

    header body = unlines $
        [ status
        , server
        , lastModified
        , etag
        , acceptRanges
        , contentLength body
        , connection
        , contentType
        ]

