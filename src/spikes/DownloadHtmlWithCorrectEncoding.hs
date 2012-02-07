import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Control.Monad.IO.Class (MonadIO)

openUrl :: String -> IO String
openUrl url = do
    bytes <- simpleHttp url 
    return $ U.toString bytes

main = do
    bs <- openUrl "http://blog.bjrn.se/2008/10/lets-build-mp3-decoder.html" 
    putStrLn bs
