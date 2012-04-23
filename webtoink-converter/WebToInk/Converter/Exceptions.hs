{-# LANGUAGE DeriveDataTypeable #-}

module WebToInk.Converter.Exceptions where

import Control.Exception (Exception)
import Data.Typeable    

data ConverterException = TableOfContentsCouldNotBeDownloadedException | UnknownDownloadException | KindlegenException Int
    deriving (Show, Typeable)

instance Exception ConverterException
