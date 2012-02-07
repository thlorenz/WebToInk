cd ../books
rm -rf Mp3 Decoder

cd ../src

runghc Converter.hs\
    --title "Mp3 Decoder" \
    --language "en-us" \
    --author "hackage" \
    --toc "http://blog.bjrn.se/2008/10/lets-build-mp3-decoder.html" \
    --folder "../books"
