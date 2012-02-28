cd ../books
rm -rf HaskellLikeC

cd ../src

runghc Converter.hs\
    --title "Haskell as Fast as C" \
    --language "en-us" \
    --author "Don Stewart" \
    --toc "http://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/" \
    --folder "../books"
