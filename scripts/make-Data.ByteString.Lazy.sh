cd ../books
rm -rf Data.ByteString.Lazy

cd ../src

runghc Converter.hs\
    --title "Data.ByteString.Lazy" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString-Lazy.html" \
    --folder "../books"
