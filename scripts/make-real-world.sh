cd ../books
rm -rf Real\ World\ Haskell

runghc ../src/HtmlBookToMobi.hs \
    --title "Real World Haskell" \
    --language "en-us" \
    --author "Bryan O'Sullivan, Don Stewart, and John Goerzen" \
    --toc "http://book.realworldhaskell.org/read/" \
