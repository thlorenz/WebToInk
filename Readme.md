# Html book to Mobi converter
Given a url to an Html book it will convert it into proper mobi format to be read on the kindle.

At this point it targets the [Real World Haskell book](http://book.realworldhaskell.org/read/)
only, but will be generalized later.

## Requirements
- [KindleGen tool](http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000234621)

## How it works
- pulls down all html pages, images and css files
- arranges them in proper folder structure and generates book.opf and toc.ncx files 
- runs KindleGen tool in order to generate the mobi file
- sample folder structure and generated files can be found at the [Learn You a
  Haskell](https://github.com/igstan/learn-you-a-haskell-kindle) project, which
accomplishes a similar goal, except not automatically and only for one book.

