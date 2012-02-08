module Converter.Types where

type Line         = String
type PageContents = String
type Url          = String

data PageInfo = PageInfo { rootUrl      :: Url
                         , pageUrl      :: Url
                         , fileName     :: FilePath
                         } deriving Show

data DownloadPagesResult = DownloadPagesResult  { allImageUrls :: [Url]
                                                , failedPages  :: [PageInfo]
                                                } deriving Show

