-- {-# LANGUAGE  #-}
module Server.Opzioni (parseArgs, Argomenti (..)) where

import System.Environment( getArgs )
import System.Directory (getDirectoryContents)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder (Permute), OptDescr (Option) , ArgDescr(NoArg,ReqArg))
import Data.List (elem)
import System.FilePath ((</>))
import Control.Arrow ((&&&))
import Lib.Tokens

data Argomenti = Argomenti
	{directory :: [(String,String)]  -- ^ directory di lavoro / nomi dei gruppi
	,porta :: Int          -- ^ porta cgi
	,lmov :: Int           -- ^ grandezza coda di movimenti di gruppo
	,lsess :: Int          -- ^ numero massimo di ricordi per sessione
	,lrem :: Int           -- ^ numero massimo di sessioni simultanee 
	,tokpass :: Token 	-- ^ password di lettura tokens	
	} deriving Show        


data Flag = Versione | Nome String | Path [(String,String)] | Porta String | LMov String | LSess String | LRem String | Tokpass String deriving Show

options :: [OptDescr Flag]
options = [
    Option "t" ["bootpassword"] (ReqArg Tokpass "PASSWORD" ) "password di lettura tokens",
    Option "V" ["versione"] (NoArg Versione)          "versione dell'applicativo",
    Option "p" ["porta"] (ReqArg Porta "PORTA") "la porta sulla quale il server CGI deve ascoltare",
    Option "g" ["lmovimenti"] (ReqArg LMov "NUMERO") "numero massimo di movimenti di gruppo in memoria",
    Option "s" ["lsessioni"] (ReqArg LSess "NUMERO") "numero massimo di sessioni in memoria",
    Option "r" ["lricordi"] (ReqArg LRem "NUMERO") "numero massimo di ricordi per sessione in memoria"
   
  ]

fallimento  =  usageInfo "Uso: reactivegas_cgi [OPZIONE ...] cartella" options

parse x r f = case reads x of 
	[(x,_)] ->  f x
	_ -> case reads ("\"" ++ x ++ "\"") of
		[(x,_)] -> f x
		_ -> r

set (Path x) r = r{directory = x}
set (Porta x) r = parse x r $ \x -> r{porta = x}
set (LMov x) r = parse x r $ \x -> r{lmov = x}
set (LSess x) r =parse x r $ \x -> r{lsess = x}
set (LRem x) r =parse x r $ \x -> r{lrem = x}
set (Tokpass x) r = parse x r $ \x -> r{tokpass = x}

-- | computa gli argomenti dell'applicazione dall'environment
parseArgs :: Argomenti -> IO Argomenti
parseArgs ars = do 
	args <- getArgs
	case getOpt Permute options args of
		(_, [],      [])     -> error $ "manca la cartella di lavoro\n" ++ fallimento
		(flags ,c : _ , [])   -> do
			paths <- filter (not . (`elem` [".",".."])) `fmap` getDirectoryContents c
			return . foldr set ars $ flags ++ [Path $ map ((c </>) &&& id ) paths]
		(_,     _,       msgs)   -> error $ concat msgs ++ fallimento


	

