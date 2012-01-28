import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Control.Exception as X

badUrl = "http://www.google.com/intl/en_com/images/srpr/WRONG.png"    

main = usingTry

xcatchReturningEmptyOnError = do
    imgData <- (simpleHttp badUrl) `X.catch` statusExceptionHandler  
    case imgData of x | x == L.empty -> return () 
                      | otherwise    -> L.writeFile "my.png" imgData

    where
        statusExceptionHandler ::  HttpException -> IO L.ByteString
        statusExceptionHandler (StatusCodeException status headers) = 
            putStr "An error occured during download: "
            >> (putStrLn $ show status)
            >> (return L.empty)

usingTry = do
    imgData <- try (simpleHttp badUrl)
    case imgData of  
        Right byteString                           -> L.writeFile "test.png" byteString
        Left  (StatusCodeException status headers) -> 
            putStr "An error occured during download: "
            >> (putStrLn $ show status)
