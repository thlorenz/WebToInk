module WebToInk.Converter.Types where

import Import

type Line         = String
type PageContents = String
type Url          = String

data PageInfo = PageInfo { piRootUrl      :: Url
                         , piPageUrl      :: Url
                         , piFileName     :: FilePath
                         } deriving Show

data DownloadPagesResult = DownloadPagesResult  { allImageUrls :: [Url]
                                                , failedPages  :: [PageInfo]
                                                } deriving Show

