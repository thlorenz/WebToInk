# Web to Mobi converter

Given a url to an online book or a blog it will convert it into proper mobi format to be read on the kindle.

## Requirements

- [Haskell Platform](http://hackage.haskell.org/platform/) (at least for now)
- [KindleGen tool](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000234621)

## Getting started

As a good example to see WebToKindle in action try converting the [Real World Haskell book](http://book.realworldhaskell.org/read/).

After cloning this repo:

    cd WebToKindle
    cabal install

    cd src
    runghc Converter.hs -c http://book.realworldhaskell.org/read/ -f ../books

Note: at this point the KindleGen tool will have to be run manually, in the
future this will be part of the automated process.

    cd ../books/Real\ World\ Haskell/
    kindlegen book.opf -o Real\ World\ Haskell/.mobi

Find some other online books to convert [here](/BooksToConvert.md)


## How it works

- pulls down all html pages, images and css files
- arranges them in proper folder structure and generates book.opf and toc.ncx files 
- runs KindleGen tool in order to generate the mobi file
- sample folder structure and generated files can be found at the [Learn You a
  Haskell](https://github.com/igstan/learn-you-a-haskell-kindle) project, which
accomplishes a similar goal, except not automatically and only for one book.

