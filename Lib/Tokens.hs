
{-# LANGUAGE Rank2Types, ScopedTypeVariables, StandaloneDeriving, TupleSections , FlexibleContexts, NoMonomorphismRestriction #-}

module Lib.Tokens where

import Control.Monad.Random (getRandom)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO, MonadIO)
import Text.ParserCombinators.ReadPrec
import Text.Read
import System.Random (Random (..))
import Control.Arrow ((***),first)

import Lib.Passo
import Lib.Response 
import Lib.Missing (Modify)

-- testing

import Lib.Console

newtype Token = Token Int deriving Eq

instance Show Token where
	show (Token n) = show n

instance Read Token where
	readPrec = Token `fmap` readS_to_Prec (const reads)

instance Random Token where
	random = (Token . (`mod` 1000000000) . abs *** id) . random
	randomR = undefined


-- | a token server
data Tokenizer m a = Tokenizer 
	{	leftTokens :: Token -> Maybe [(Token,a)]
	-- | given a token, use it if it is valid. Nothing signal an invalid token. otherwise compute a new tokenizer without the use one.
	, 	useToken :: Token -> Maybe (a,Tokenizer m a)
	-- | add new tokens to the token pool. The random monad is used to extract random tokens from it. Nothing signals a wrong password
	,	generateTokens :: MonadIO m => Token -> Int -> Maybe ((Token -> m a) -> m (Tokenizer m a))
	-- | expose internals. Should not be used to show , but it's simmetric to read :)
	,	serialize :: (Token,[(Token,a)])
	}

-- | build a tokenizer from an amministrator token and a list of give tokens
mkTokenizer :: (MonadIO m, Show a) => Token -> [(Token,a)] -> Tokenizer m a
mkTokenizer p0 zs = let
	to x = Tokenizer (leftTokens_ x) (useToken_ x) (generateTokens_ x) (p0,x)
	leftTokens_ xs p = if p /= p0 then Nothing
		else Just xs
	useToken_ xs x = fmap (snd *** to) $ dropfst xs ((==x) . fst)
	generateTokens_ xs p n = if p /= p0 then Nothing
			else Just $ \c -> do
				ts <- liftIO $ replicateM n getRandom
				as <- mapM c $ ts
				return . to $ xs ++ zip ts as
	in to zs

-- | a single operation filter, removing only the first match 
dropfst 	:: [a] 		-- ^ list to be filtered
		-> (a -> Bool)  -- ^ filtering condition
		-> Maybe (a,[a])		-- ^ filtered list if element found
dropfst xs c = let (y,z) = break c xs in if null z then Nothing else Just $ (head z, y ++ tail z)



uiTokenizer 
  :: forall m c b . (Show c , MonadIO m, Functor m) 
	=> Modify (Costruzione m b) (Tokenizer (Costruzione m b) c)
	-> (Token -> Costruzione m b c)
 	-> (((c -> Costruzione m b ()) -> Costruzione m b ()) -> Costruzione m b ())
     	-> [(String,Costruzione m b ())] 

uiTokenizer tokens notante accessed = [
	("utilizza un token", accessed $ \f -> do
			t' <- libero "il token da utilizzare" 
			tokens $ \tk -> case useToken tk t' of
					Nothing -> do 
						errore $ ResponseOne "il token non è valido"
						return Nothing
					Just (n,tk') -> f n >> return (Just tk')
			),
	("token inutilizzati", do
			t <- password "inserimento token di amministrazione"
			tokens $ \tk -> do
				case leftTokens tk t of
					Nothing -> errore $ ResponseOne "il token di amministrazione è un altro"
					Just ts -> output $ ResponseAL [("elenco tokens inutilizzati",
						ResponseAL $ map (first show) ts)]
				return Nothing
	 
			),
	("generazione nuovi token", do
			n <- libero "numero di token da aggiungere"
			t <- password "il token di amministrazione"
			tokens $ \tk -> case generateTokens tk t n of
				Nothing -> do 
					errore $ ResponseOne "il token di amministrazione è un altro"
					return Nothing
				Just f -> Just `fmap` f notante
			)
	]
uiTokenizerCheck  :: Monad m => Modify (Costruzione m b) (Tokenizer (Costruzione m b) c) -> Costruzione m b () -> Costruzione m b () 
uiTokenizerCheck tokens cont = do
	 	t <- password "inserimento token di amministrazione"
		tokens $ \tk -> do 
			if fst (serialize tk) == t then cont else 
				errore $ ResponseOne "il token di amministrazione è un altro"
			return Nothing
