{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | modulo per la gestione dei conti utente e responsabile, detti accredito e saldo
module Eventi.Accredito {-(
                        --	Accredito,
                        	Conti,
                        	Saldi,
                        	preleva,
                        	accredita,
                        	salda,
                        	reazioneAccredito ,
                        	statoInizialeAccredito ,
                        	makeAccredito,
                        	priorityAccredito,
                        	queryAccredito
                        	)-} where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad (when)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (MonadReader, asks)
import Core.Costruzione (CostrAction, libero, runSupporto, scelte)
import Core.Dichiarazioni (Dichiarazione (Singola), Singola)
import Core.Inserimento (MTInserzione, fallimento, logga, loggamus, modifica, osserva)
import Core.Parsing (Parser, ParserConRead (ParserConRead))
import Core.Programmazione (Message (..), Reazione, nessunEffetto, soloEsterna)
import Core.Types (Utente)
import Data.Maybe (fromJust, isNothing)
import Data.Typeable (Typeable, cast)
import Eventi.Anagrafe
  ( Anagrafe
  , Responsabili
  , SUtente (..)
  , esistenzaResponsabile
  , esistenzaUtente
  , responsabili
  , utenti
  , validante
  )
import Lib.Aspetti (ParteDi, see, (.<))
import Lib.Assocs (update, upset, (?))
import Lib.Euro
import Lib.Missing (sortByLower, sortLower)
import Lib.Prioriti (R (..))
import Lib.Response (Response (..))
import Lib.ShowRead

data Movimento = MovimentoU Utente DEuro String | MovimentoR Utente DEuro String deriving (Typeable)

instance Show Movimento where
  show (MovimentoU u e s) = "movimento di " ++ show e ++ " sul conto di " ++ u ++ " per " ++ s
  show (MovimentoR u e s) = "movimento di " ++ show e ++ " sulla cassa di " ++ u ++ " per " ++ s

-- | evento esterno che interessa il controllo del credito o del saldo
data EsternoAccredito = Accredito Utente DEuro | Addebito Utente String DEuro | Saldo Utente DEuro

instance Show EsternoAccredito where
  show (Accredito u f) = "accredito a " ++ quote u ++ " di " ++ show f
  show (Saldo u f) = "movimento dalla cassa di " ++ quote u ++ " di " ++ show f
  show (Addebito u s f) = "addebito a " ++ quote u ++ " di " ++ show f ++ " per " ++ quote s

instance Read EsternoAccredito where
  readPrec =
    let acc = do
          string "accredito a "
          u <- phrase
          string " di "
          f <- reads'
          return $ Accredito u f
        add = do
          string "addebito a "
          u <- phrase
          string " di "
          f <- reads'
          string " per "
          s <- phrase
          return $ Addebito u s f
        sal = do
          string "movimento dalla cassa di "
          u <- phrase
          string " di "
          f <- reads'
          return $ Saldo u f
     in lift $ acc <++ sal <++ add

-- | priorita' per gli eventi del modulo
priorityAccredito :: R
priorityAccredito = R k
  where
    k (Accredito _ _) = -35
    k (Saldo _ _) = -35
    k (Addebito _ _ _) = -34

-- | stato degli accrediti utente
data Conti = Conti [(Utente, Euro)] deriving (Read, Show, Eq)

conti (Conti us) = sortByLower fst us

-- | stato dei saldi responsabile
data Saldi = Saldi [(Utente, Euro)] deriving (Read, Show, Eq)

saldi (Saldi us) = sortByLower fst us

reportCrediti :: (Conti `ParteDi` s) => s -> [(Utente, Euro)]
reportCrediti = (\(Conti xs) -> sortByLower fst xs) . see

reportCasse :: (Saldi `ParteDi` s) => s -> [(Utente, Euro)]
reportCasse = (\(Saldi xs) -> sortByLower fst xs) . see

-- | tipo aggiunto dello stato necessario al modulo
type TyAccredito a = (Conti, (Saldi, a))

-- | aggiunge lo stato del modulo allo stato passato
bootAccredito :: a -> TyAccredito a
bootAccredito x = Conti [] .< Saldi [] .< x

-- | esegue un accredito su un conto utente
accredita :: (Anagrafe `ParteDi` s, Conti `ParteDi` s) => Utente -> DEuro -> String -> MTInserzione s c Utente Euro
accredita u dv s = do
  esistenzaUtente u
  Conti us <- osserva
  let r = dv $^ (us ? (u, 0))
  modifica $ \(Conti us) -> Conti (upset u r us)
  logga . Message $ MovimentoU u dv s
  return r

-- | modifica il saldo di un responsabile
salda
  :: (Anagrafe `ParteDi` s, Responsabili `ParteDi` s, Saldi `ParteDi` s)
  => Utente
  -> DEuro
  -> String
  -> MTInserzione s c Utente ()
salda u dv s = do
  esistenzaResponsabile u
  modifica $ \(Saldi us) -> Saldi (update u (dv $^) 0 us)
  logga . Message $ MovimentoR u dv s

-- | il caricatore di eventi per questo modulo
reazioneAccredito
  :: ( Responsabili `ParteDi` s
     , Saldi `ParteDi` s
     , Anagrafe `ParteDi` s
     , Conti `ParteDi` s
     , Parser c EsternoAccredito
     )
  => Reazione s c Utente
reazioneAccredito = soloEsterna reattoreAccredito
  where
    reattoreAccredito (first validante -> (wrap, Accredito u dv)) = wrap $ \r -> do
      fallimento (r == u) "aggiornamento del proprio credito di utente"
      when (dv $^ 0 <= 0) $ loggamus "accredito negativo"
      accredita u dv $ "versamento presso il cassiere " ++ quote r
      salda r dv $ "funzione di cassiere per " ++ quote u
      loggamus $ "accreditate " ++ show dv ++ " a " ++ quote u
      return (True, nessunEffetto)
    reattoreAccredito (first validante -> (wrap, Addebito u s dv)) = wrap $ \r -> do
      fallimento (r == u) "aggiornamento del proprio credito di utente"
      when (dv $^ 0 <= 0) $ loggamus "prelievo negativo o nullo"
      z <- accredita u (opposite dv) $ "prelievo attraverso il cassiere " ++ quote r ++ " per " ++ quote s
      when (z < 0) $ loggamus "il credito non copre l'operazione"
      salda r (opposite dv) $ "funzione di cassiere per " ++ quote u
      loggamus $ "prelevate " ++ show dv ++ " a " ++ quote u ++ " per " ++ s
      return (True, nessunEffetto)
    reattoreAccredito (first validante -> (wrap, Saldo u dv)) = wrap $ \r -> do
      esistenzaResponsabile u
      fallimento (u == r) "movimento di denaro riferito ad una cassa sola"
      fallimento (dv $^ 0 <= 0) "saldo negativo o nullo"
      salda r dv $ "trasferiti dalla cassa di " ++ quote u
      salda u (opposite dv) $ "trasferiti alla cassa di " ++ quote r
      loggamus $ "spostati " ++ show dv ++ " dalla cassa di " ++ quote u ++ " alla cassa di " ++ quote r
      return (True, nessunEffetto)

-- | costruttore di eventi per il modulo di accredito
costrEventiAccredito
  :: (Parser p EsternoAccredito, Monad m, ParteDi Responsabili s, SUtente `ParteDi` s, ParteDi Anagrafe s)
  => CostrAction m c (Dichiarazione p Singola) s
costrEventiAccredito s kp kn =
  [ ("versamento", eventoAccredito)
  , ("prelievo motivato", eventoAddebito)
  , ("movimento cassa", eventoSaldo)
  ]
  where
    run = runSupporto s kn kp
    eventoAccredito = run $ do
      us <- asks utenti
      SUtente un <- asks see
      when (isNothing un) $ throwError "manca la scelta del responsabile autore"
      let us' = map (id &&& id) $ filter ((/=) $ fromJust un) us
      when (null us') $ throwError "non ci sono altri utenti"
      u <- scelte us' $ ResponseOne "utente interessato dall'aggiornamento"
      n <- libero $ ResponseOne $ "somma da accreditare sul conto di " ++ quote u
      return . Singola $ Accredito u n
    eventoAddebito = run $ do
      us <- asks utenti
      SUtente un <- asks see
      when (isNothing un) $ throwError "manca la scelta del responsabile autore"
      let us' = map (id &&& id) $ filter ((/=) $ fromJust un) us
      when (null us') $ throwError "non ci sono altri utenti"
      u <- scelte us' $ ResponseOne "utente interessato dall'aggiornamento"
      n <- libero $ ResponseOne $ "somma da prelevare dal conto di " ++ quote u
      s <- libero $ ResponseOne $ "motivazione del prelievo"
      return . Singola $ Addebito u s n
    eventoSaldo = run $ do
      (rs, _) <- asks responsabili
      SUtente un <- asks see
      when (isNothing un) $ throwError "manca la scelta del responsabile autore"
      let rs' = map (fst &&& id) (filter ((/=) (fromJust un) . fst) rs)
      when (null rs') $ throwError "non ci sono altri responsabili"
      u <- scelte rs' $ ResponseOne "responsabile che ha dato il denaro"
      n <- libero $ ResponseOne $ "somma ricevuta dal responsabile " ++ quote (fst u)
      return . Singola $ Saldo (fst u) n

-- | costruttore interrogazioni sul modulo accrediti
costrQueryAccredito :: (Monad m, Conti `ParteDi` s, Saldi `ParteDi` s) => CostrAction m c Response s
costrQueryAccredito s kp kn =
  [ ("crediti degli utenti", queryUtente)
  , ("casse dei responsabili", queryResponsabile)
  ]
  where
    run = runSupporto s kn kp
    queryUtente = run $ do
      us <- asks (conti . see)
      return $
        Response
          [ ( "crediti degli utenti"
            , if null us
                then ResponseOne "nessun utente possiede un credito"
                else ResponseMany (map (ResponseOne *** id) us)
            )
          ]
    queryResponsabile = run $ do
      rs <- asks (saldi . see)
      return $
        Response
          [ ( "casse dei responsabili"
            , if null rs
                then ResponseOne "nessun responsabile possiede una cassa attiva"
                else ResponseMany (map (ResponseOne *** id) rs)
            )
          ]
