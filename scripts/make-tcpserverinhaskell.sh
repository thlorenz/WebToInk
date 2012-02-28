cd ../books
rm -rf TCP\ Server\ Haskell

cd ../src

runghc Converter.hs\
    --title "TCP Server Haskell" \
    --language "en-us" \
    --author "Peteris Krumins" \
    --toc "http://www.catonmat.net/blog/simple-haskell-tcp-server/" \
    --folder "../books"
