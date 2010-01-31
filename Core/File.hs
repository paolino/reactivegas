module Core.File where

import Data.List (intercalate)
import Control.Exception (handle, SomeException (..))

readEvents :: IO [String]
readEvents = do
	l <- handle (\(SomeException _) -> return "") $ readFile "patch" 
	return $ lines l

writeEvents :: [String] -> IO ()
writeEvents xs = writeFile "patch" $ intercalate "\n" xs
