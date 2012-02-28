cd ../books
rm -rf Data.ByteString

cd ../src

runghc Converter.hs\
    --title "Data.ByteString" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString.html" \
    --folder "../books"
