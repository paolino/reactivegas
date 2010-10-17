
{-# LANGUAGE Rank2Types, ScopedTypeVariables, StandaloneDeriving, TupleSections , FlexibleContexts, NoMonomorphismRestriction #-}

module Lib.Tokens where

import Control.Monad.Random (getRandom)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO, MonadIO)
import Text.ParserCombinators.ReadPrec
import Text.Read
import System.Random (Random (..))
import Control.Arrow ((***),first)
import Data.Typeable (Typeable)

import Lib.Passo
import Lib.Response 
import Lib.Modify (PeekPoke (peek), modifyT)

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
data Tokenizer a b = Tokenizer 
	{
	-- | tokens waiting
		leftTokens :: Token -> Maybe [(Token,b)]
	-- | elements of received tokens
	, 	promotedElements :: [(b,a)]	
	-- | given a token, use it if it is valid. Nothing signal an invalid token. otherwise compute a new tokenizer without the use one.
	, 	useToken :: Token -> Maybe ((Token,b), a -> Tokenizer a b)
	-- | add new tokens to the token pool. The random monad is used to extract random tokens from it. Nothing signals a wrong password
	,	generateTokens :: Token -> b -> Maybe (IO (Tokenizer a b))
	-- | expose internals. Should not be used to show , but it's simmetric to read :)
	,	serialize :: (Token,[(Token,b)],[(b,a)])
	}
-- | build a tokenizer from an amministrator token and a list of give tokens
mkTokenizer :: (Show a) => (Token,[(Token,b)],[(b,a)]) -> Tokenizer a b
mkTokenizer (p0,zs,as) = let
	to xs as = Tokenizer (leftTokens_ xs) (promotedElements_ as) (useToken_ xs as) (generateTokens_ xs as) (p0,xs,as)
	leftTokens_ xs p = if p /= p0 then Nothing
		else Just xs
	promotedElements_ as = as
	useToken_ xs as x = do 
		((t,b),xs') <- dropfst xs ((==x) . fst)
		return $ ((t,b),\a -> to xs' ((b,a):as))
	generateTokens_ xs as p b = if p /= p0 then Nothing
			else Just $ do
				t <- getRandom
				return $ to ((t,b):xs) as
	in to zs as

-- | a single operation filter, removing only the first match 
dropfst 	:: [a] 		-- ^ list to be filtered
		-> (a -> Bool)  -- ^ filtering condition
		-> Maybe (a,[a])		-- ^ filtered list if element found
dropfst xs c = let (y,z) = break c xs in if null z then Nothing else Just $ (head z, y ++ tail z)



uiTokenizer 
  :: forall m c b d . (Typeable d , Read d, Show d,Read c, Show c , MonadIO m, Functor m) 
	-- | tokenizer che raccolglie elementi c
	=> PeekPoke (Tokenizer c d)
	-- | presentazione elenco elementi raccolti
	-> String 				
	-- | interazione di creazione valore associato vincolato
	-> (Costruzione m b d) 	
	-- | utilizzatore dell'elemento raccolto		
 	-> ((((Token,d) -> Costruzione m b c) -> Costruzione m b ()) -> Costruzione m b ())
	-- | procedura di finalizzazione tokenizer
	-> ([c] -> Costruzione m b ())
	-- | menu
     	-> [(String,Costruzione m b ())] 

uiTokenizer tokens notedrv notante accessed close = [
	("utilizza un token", accessed $ \f -> do
			t' <- libero "il token da utilizzare" 
			modifyT tokens $ \tk -> case useToken tk t' of
					Nothing -> do 
						errore $ ResponseOne "il token non è valido"
						return Nothing
					Just (b,g) -> f b >>= \h -> return (Just $ g h)
			),
	(notedrv,liftIO (peek tokens) >>= \tk -> do
		let (ds,cs) = unzip $ promotedElements tk
		output $ ResponseAL [(notedrv , ResponseMany $ map ResponseOne ds)]),
	("token inutilizzati", do
			t <- libero "inserimento token di amministrazione"
			modifyT tokens $ \tk -> do
				case leftTokens tk t of
					Nothing -> errore $ ResponseOne "il token di amministrazione è un altro"
					Just ts -> output $ ResponseAL $ map (show *** ResponseOne) ts
				return Nothing
	 
			),
	("generazione nuovo token", do
			a <- notante
			t <- libero "il token di amministrazione"
			modifyT tokens $ \tk -> case generateTokens tk t a of
				Nothing -> do 
					errore $ ResponseOne "il token di amministrazione è un altro"
					return Nothing
				Just g -> Just `fmap` liftIO g
					

			),
	("chiusura raccolta tokens", do
			t <- libero "il token di amministrazione"
			liftIO (peek tokens) >>= \tk -> 
				case leftTokens tk t of
					Nothing -> errore $ ResponseOne "il token di amministrazione è un altro"
					Just _ -> close $ map snd $ promotedElements tk)
	]
