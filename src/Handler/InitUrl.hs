module Handler.InitUrl where

import Import
import Data.Text (unpack)

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- runInputGet $ ireq textField "urlText"
    jsonToRepJson . object . toTextPairs $ 
        [ ("title"  , "the title")
        , ("url"    , unpack url)
        ] 
