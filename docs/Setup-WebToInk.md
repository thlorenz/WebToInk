## Clone

    git clone git://github.com/thlorenz/dotfiles.git

## Prepare cabal
    
    cabal update

## Install WebToInk-Converter

    cd webtoink-converter
    cabal install

## Install WebToInk
    
    cd ../webtoink
    cabal install

If this fails most times (should be fixed, cause I changed required http-enumerator version in webtoink-converter), 
we need to do:

    cabal install zlib-conduit zlib-bindings --reinstall

