module UI where
import Control.Applicative
import Data.Maybe

untilIn :: Show a => String -> [a] -> IO a
untilIn s ls = let picks = zip [1..] ls in
	mapM_ putStr (map (\(i,v) -> show i ++ "- "++ show v ++ "\n")  picks) >> untilParse s >>= \w -> if w <= length ls && w > 0 then return (fromJust $ lookup w picks) else untilIn s ls

untilParse :: Read a => String -> IO a
untilParse s = do 
	putStr (s ++ ": ")
	rs <- reads <$> getLine
	case rs of
		[] -> untilParse s 
		qs -> return . fst . last $ qs 

untilNotIn s ls = untilParse s >>= \w -> if not $ w `elem` ls then return w else untilIn s ls


