import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Control.Exception as X

badUrl = "http://www.google.com/intl/en_com/images/srpr/WRONG.png"    

oldmain = do
    imgData <- (simpleHttp badUrl) `X.catch` statusExceptionHandler  
    case imgData of x | x == L.empty -> return () 
                      | otherwise    -> L.writeFile "my.png" imgData

    where
        statusExceptionHandler ::  HttpException -> IO L.ByteString
        statusExceptionHandler (StatusCodeException status headers) = 
            putStr "An error occured during download: "
            >> (putStrLn $ show status)
            >> (return L.empty)

main = do
    let imgData = getImgData
    case imgData of  Just x  -> x >>= L.writeFile "test.png"
                     Nothing -> return ()
    where
        getImgData :: Maybe (IO L.ByteString)
        getImgData =  (Just $ simpleHttp badUrl) -- `X.catch` statusExceptionHandler 

        statusExceptionHandler :: HttpException -> Maybe (IO L.ByteString)
        statusExceptionHandler (StatusCodeException status headers) = Nothing
    


