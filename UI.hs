module UI where
import Control.Applicative

untilIn :: (Eq a, Read a) => String -> [a] -> IO a
untilIn s ls = untilParse s >>= \w -> if w `elem` ls then return w else untilIn s ls

untilParse :: Read a => String -> IO a
untilParse s = do 
	putStr s 
	rs <- reads <$> getLine
	case rs of
		[] -> untilParse s 
		qs -> return . fst . last $ qs 

untilNotIn s ls = untilParse s >>= \w -> if not $ w `elem` ls then return w else untilIn s ls


