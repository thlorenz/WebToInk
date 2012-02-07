import Network.HTTP.Enumerator (simpleHttp)
import qualified Data.ByteString.Lazy as L

-- openUrl :: Control.Monad.IO.Class.MonadIO m => String -> m L.ByteString
openUrl url = simpleHttp url

main = do
    openUrl "http://blog.bjrn.se/2008/10/lets-build-mp3-decoder.html" >>= L.putStrLn
