-- {-# LANGUAGE  #-}
module Server.Opzioni (parseArgs, Argomenti (..)) where

import System.Environment( getArgs )
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder (Permute), OptDescr (Option) , ArgDescr(NoArg,ReqArg))

data Argomenti = Argomenti
	{directory :: String  -- ^ directory di lavoro / nome del gruppo
	,porta :: Int          -- ^ porta cgi
	,lagg :: Int           -- ^ grandezza coda di aggiornamenti di gru
	,lsess :: Int          -- ^ numero massimo di ricordi per sessione
	,lrem :: Int           -- ^ numero massimo di sessioni simultanee 
	,tokpass :: String 	-- ^ password di lettura tokens	
	} deriving Show        


data Flag = Versione | Nome String | Path String | Porta String | LAgg String | LSess String | LRem String | Tokpass String deriving Show

options :: [OptDescr Flag]
options = [
    Option "t" ["bootpassword"] (ReqArg Tokpass "PASSWORD" ) "password di lettura tokens",
    Option "V" ["versione"] (NoArg Versione)          "versione dell'applicativo",
    Option "p" ["porta"] (ReqArg Porta "PORTA") "la porta sulla quale il server CGI deve ascoltare",
    Option "g" ["laggiornamenti"] (ReqArg LAgg "NUMERO") "numero massimo di aggiornamenti di gruppo in memoria",
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
set (LAgg x) r = parse x r $ \x -> r{lagg = x}
set (LSess x) r =parse x r $ \x -> r{lsess = x}
set (LRem x) r =parse x r $ \x -> r{lrem = x}
set (Tokpass x) r = parse x r $ \x -> r{tokpass = x}

-- | computa gli argomenti dell'applicazione dall'environment
parseArgs :: Argomenti -> IO Argomenti
parseArgs ars = do 
	args <- getArgs
	case getOpt Permute options args of
		(_, [],      [])     -> error $ "manca la cartella di lavoro\n" ++ fallimento
		(flags ,c : _ , [])   -> return . foldr set ars $ flags ++ [Path c]
		(_,     _,       msgs)   -> error $ concat msgs ++ fallimento


	

