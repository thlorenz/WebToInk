# assume we are in the scripts folder

cd ../books
rm -rf Real\ World\ Haskell

cd ../src

runghc Converter.hs \
    --title "Real World Haskell" \
    --language "en-us" \
    --author "Bryan O'Sullivan, Don Stewart, and John Goerzen" \
    --toc "http://book.realworldhaskell.org/read/" \
    --folder "../books"
