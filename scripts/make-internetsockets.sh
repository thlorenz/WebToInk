cd ../books
cd ../src

runghc Converter.hs\
    --title "Beej's Guide to Network Programming" \
    --language "en-us" \
    --author "Brian Hall" \
    --toc "http://beej.us/guide/bgnet/output/html/multipage/index.html" \
    --folder "../books"
