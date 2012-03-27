module Handler.Utils where

import Data.Text (pack, unpack)

import Import

toTextPairs :: [(String, String)] -> [(Text, Text)]
toTextPairs = map toTextPair
  where toTextPair (a, b) = (pack a, pack b)

getStringFromField ::  Text -> GHandler sub WebToInk String
getStringFromField = fmap unpack . runInputGet . ireq textField 
