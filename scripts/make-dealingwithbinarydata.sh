cd ../books
rm -rf DealingWithBinaryData

cd ../src

runghc Converter.hs\
    --title "Dealing with binary data" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://www.haskell.org/haskellwiki/DealingWithBinaryData" \
    --folder "../books"
