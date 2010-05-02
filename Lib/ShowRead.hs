{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib.ShowRead (phrase, readPrec, lift,string, (<++), quote, reads') where

import Text.ParserCombinators.ReadP
import Text.Read hiding (get,(<++))

charOrEscape = (char '\\' >> get) <++ get
phrase = char '"' >> manyTill charOrEscape (char '"')
quote x = "\"" ++ x ++ "\""
reads' = readS_to_P reads
