{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module UI.Lib where

import Data.List (delete, find, lookup, (\\))
import Data.Maybe (catMaybes, fromJust, isJust)

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

import Lib.Passo (Costruzione, Passo, mano, menu, rmenu, rotonda)
import qualified Lib.Passo as P

import Lib.Firmabile (Chiave, cryptobox)
import Lib.Prioriti (R)
import Lib.Response (Response (..))
import Lib.TreeLogs (eccoILogs)

import Core.Contesto (flatten)
import Core.Controllo (SNodo (..), caricaEventi)
import Core.Costruzione (Supporto, runSupporto)
import Core.Parsing (ParserConRead)
import Core.Patch (Firmante (..), Patch, firmante, fromPatch)
import Core.Programmazione (Reazione)
import Core.Types (Esterno, Evento, Responsabile, SessioneAcquisto (..), SessioneOrdinante (..), Utente)

import Eventi.Accredito
import Eventi.Acquisto
import Eventi.Anagrafe
import Eventi.Impegno

import Applicazioni.Amministratore (Administrator, getGroupValue)
import Applicazioni.Persistenza (Persistence (..))
import Applicazioni.Reactivegas (Effetti, QS (..), TS, bianco, levelsEventi, maxLevel, sortEventi)
import Applicazioni.Sessione (Sessione (..))

fJ y x = case x of Nothing -> error (show y); Just x -> x

daChiave :: Chiave -> [Responsabile] -> Maybe Responsabile
daChiave c = find (\(_, (c', _)) -> c == c')

eventi :: [(Utente, Patch)] -> [(Utente, [Evento])]
eventi = map (second $ \(_, _, es) -> es)

-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO

type Name = String

type Environment =
    ( Administrator (Persistence QS Effetti Response)
    , Sessione QS Response ParserConRead
    )

statoPersistenza :: (Functor m, MonadReader Environment m, MonadIO m) => m QS
statoPersistenza = snd `fmap` sepU readState

statoSessione = fmap (fJ "stato sessione") . sel $ readStatoSessione . snd

ses f = sel $ f . snd
sepU f = sel $ \(pe, se) -> do
    g <- readGruppo se
    pe' <- maybe (return Nothing) (atomically . getGroupValue pe) g
    f . fromJust $ pe'
sep f = sel $ \(pe, se) -> do
    g <- readGruppo se
    pe' <- maybe (return Nothing) (atomically . getGroupValue pe) g
    f pe'
sea f = sel $ f . fst

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv = ReaderT Environment IO

type Interfaccia a = Costruzione MEnv () a

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> String -> Interfaccia a
bocciato s x = do
    P.errore True . Response $ [(s, ResponseOne x)]
    return undefined

bocciatoS s x = P.errore True . Response $ [(s, ResponseOne x)]

accesso :: Interfaccia ()
accesso = do
    (rs, _) <- responsabili . fst . unQS <$> statoPersistenza
    r <- P.scelte (("<anonimo>", Nothing) : map (fst &&& Just) rs) $ ResponseOne "responsabile autore delle dichiarazioni"
    ses $ flip writeAccesso r

onAccesso k = ses readAccesso >>= maybe (accesso >> onAccesso k) k

creaChiavi = do
    p1 <- P.password "immetti una password, una frase , lunga almeno 12 caratteri"
    p2 <- P.password "reimmetti la password per controllare la digitazione"
    if p1 == p2
        then
            P.download "nuove.chiavi" "scarica delle nuove chiavi" $ cryptobox p1
        else bocciato "creazione chiavi" "errore di digitazione password"

letturaEventi :: Interfaccia [Evento]
letturaEventi = ses readEventi

addEvento x = ses $ flip aggiungiEvento x
addEventoC x = ses $ flip correggiEvento x
eventLevelSelector = do
    (_, us) <- sepU readUserPatches
    es' <- letturaEventi
    mu <- ses readAccesso
    let es = levelsEventi . (es' ++) . concatMap snd . maybe id (\(u, _) -> filter ((/=) u . fst)) mu $ eventi us
    let rs = case es of
            [] -> Nothing
            es -> Just $ (const "<nessuno>" *** subtract 1) (head es) : es ++ [("<tutti>", maxLevel)]
    case rs of
        Nothing -> bocciato "selezione livello di considerazione" "nessuna dichiarazione presente"
        Just rs ->
            mano (ResponseOne "livello di considerazione delle ultime dichiarazioni") $
                map
                    ( \(x, l) ->
                        (x, ses (`setConservative` l))
                    )
                    rs

eliminazioneEvento :: String -> Interfaccia ()
eliminazioneEvento s = do
    es <- ses eliminaEvento
    if null es
        then bocciato s "non ci sono dichiarazioni da eliminare"
        else menu (ResponseOne "seleziona una dichiarazione da eliminare") $ map (second liftIO) es

salvataggio s = do
    evs <- letturaEventi
    onAccesso $ \r@(u, _) -> do
        let p up = sepU (\pe -> writeUserPatch pe u up)
            k (Firmante f) = do
                evs <- letturaEventi
                statoPersistenza >>= (\s -> p (f s evs)) . fst . unQS
        runSupporto (fst . unQS <$> statoPersistenza) (bocciato s) k $ firmante r

sincronizza = onAccesso $ \r@(u, _) -> do
    (_, rs) <- second (map snd) <$> sepU readUserPatches
    case rs of
        [] -> bocciato "sincronizzazione gruppo" "nessun aggiornamento individale per lo stato attuale"
        xs -> do
            let k (Firmante f) = statoPersistenza >>= (\s -> sepU $ flip writeGroupPatch (f s xs)) . fst . unQS
            runSupporto (fst . unQS <$> statoPersistenza) (bocciato "sincronizzazione gruppo") k $ firmante r

{-
caricaAggiornamentoIndividuale :: Interfaccia ()
caricaAggiornamentoIndividuale = do
	p@(c,_,_) <- P.upload  "aggiornamento individuale"
	s <- fst <$> unQS <$> statoPersistenza
	rs <- runExceptT . flip runReaderT s $ fromPatch (fst . responsabili) p
	case rs of
		Left prob -> bocciato "caricamento aggiornamento individuale" prob
		Right _ -> do
			let Just (u,_) = daChiave c (fst $ responsabili s)
			sepU $ ($p) . ($u) . writeUserPatch

scaricaAggiornamentoIndividuale :: Interfaccia ()
scaricaAggiornamentoIndividuale = do
	(_,us) <- sepU readUserPatches
	(u,p) <- P.scelte  (map (fst &&& id) us)  $ ResponseOne "aggiornamenti utente presenti"
	v <- sepU readVersion
	P.download  (u ++ "." ++ show v) "scarica un aggiornamento individuale" p

caricaAggiornamentoDiGruppo :: Interfaccia ()
caricaAggiornamentoDiGruppo = P.upload  "aggiornamento di gruppo" >>= \g -> sepU $ ($g). writeGroupPatch

scaricaAggiornamentoDiGruppo :: Interfaccia ()
scaricaAggiornamentoDiGruppo = do
	n <- P.libero  $ ResponseOne  "indice dell'aggiornamento richiesto"
	mg <- sepU $ ($n) . readGPatch
	case mg of
		Nothing -> bocciato "scaricamento aggiornamento di gruppo"  "aggiornamento di gruppo non trovato"
		Just g -> P.download  ("group." ++ show n) "scarica un aggiornamento individuale" g

effetto = do
	c <- fJ "6.UI.Server" <$> ses readCaricamento
	P.output False . Response $ [("effetto delle ultime dichiarazioni",  c)]
-}
