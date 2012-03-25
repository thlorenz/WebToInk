module Handler.Utils where

import Data.Text (Text, pack)
import Data.List (map)
import Data.Char (String)

toTextPairs :: [(String, String)] -> [(Text, Text)]
toTextPairs = map toTextPair
  where toTextPair (a, b) = (pack a, pack b)
