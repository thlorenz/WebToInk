cd ../books
rm -rf Learn\ You\ a\ Haskell

runghc ../src/HtmlBookToMobi.hs \
    --title "Learn You a Haskell" \
    --language "en-us" \
    --author "Miran Lipovaca" \
    --toc "http://learnyouahaskell.com/chapters" \
