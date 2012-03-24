module Handler.InitUrl where

import Data.Text (pack)
import Import

getInitUrlR :: Handler RepJson
getInitUrlR = do
    url <- runInputGet $ ireq textField "urlText"
    jsonToRepJson . object . toTextPairs $ [("title", "the title")] 

toTextPairs :: [(String, String)] -> [(Text, Text)]
toTextPairs = map toTextPair
  where toTextPair (a, b) = (pack a, pack b)
