module Handler.Utils where

import Data.Text (Text, pack, unpack)
import Data.List (map)
import Data.Char (String)

import Import

toTextPairs :: [(String, String)] -> [(Text, Text)]
toTextPairs = map toTextPair
  where toTextPair (a, b) = (pack a, pack b)

getStringFromField ::  Text -> GHandler sub WebToInk String
getStringFromField = fmap unpack . runInputGet . ireq textField 
