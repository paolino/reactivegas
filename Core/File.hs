module Core.File where

import Data.List (intercalate)
import Control.Exception (handle, SomeException (..))

readEvents :: IO [String]
readEvents = do
	l <- handle (\(SomeException _) -> return "") $ readFile "eventi" 
	return $ lines l

writeEvents :: [String] -> IO ()
writeEvents xs = writeFile "eventi" $ intercalate "\n" xs
