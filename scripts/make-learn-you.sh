cd ../books
rm -rf Learn\ You\ a\ Haskell

cd ../src

runghc Converter.hs \
runghc Converter.hs\
    --title "Learn You a Haskell" \
    --language "en-us" \
    --author "Miran Lipovaca" \
    --toc "http://learnyouahaskell.com/chapters" \
    --folder "../books"
