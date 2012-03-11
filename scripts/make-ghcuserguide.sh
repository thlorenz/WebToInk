#!/bin/sh

script_dir=$(dirname $0)
title = "GHC User Guide"

cd $script_dir/../books

rm -rf ./$title

cd ../src

runghc Converter.hs\
    --title $title \
    --language "en-us" \
    --author "GHC Team" \
    --toc "http://www.haskell.org/ghc/docs/7.2.1/html/users_guide" \
    --folder $script_dir/../books

cd ./$title

kindlegen  book.opf
