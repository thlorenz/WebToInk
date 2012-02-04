cd ../books
rm -rf Data\ List

runghc ../src/HtmlBookToMobi.hs \
    --title "Data List" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html" \
