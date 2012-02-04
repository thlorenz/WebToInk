cd ../books
rm -rf Data\ List

cd ../src

runghc Converter.hs\
    --title "Data List" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html" \
    --folder "../books"
