cd ../books
rm -rf Dropbox API

cd ../src

runghc Converter.hs\
    --title "Dropbox API" \
    --language "en-us" \
    --author "Rian" \
    --toc "http://tech.dropbox.com/?p=129" \
    --folder "../books"
