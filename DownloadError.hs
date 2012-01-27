import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Control.Exception as X

main = do
    let badUrl = "http://www.google.com/intl/en_com/images/srpr/WRONG.png"    
    imgData <- (simpleHttp badUrl) `X.catch` statusExceptionHandler  
    L.writeFile "my.png" imgData

statusExceptionHandler ::  HttpException -> IO L.ByteString
statusExceptionHandler (StatusCodeException status headers) = 
    (putStrLn $ show status) >> (putStrLn $ show headers) >> (return L.empty)
