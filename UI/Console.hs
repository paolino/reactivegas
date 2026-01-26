{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module UI.Console (interfaccia) where

import Data.List (delete, find, (\\))
import Data.Maybe (catMaybes, fromJust, isJust)

import Control.Applicative
import Control.Arrow
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

import Lib.Passo (Costruzione, HPasso, Passo, mano, menu, rmenu, rotonda, svolgi)
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
import Core.Types (Esterno, Evento, Responsabile, Utente)

import Eventi.Accredito
import Eventi.Acquisto
import Eventi.Anagrafe
import Eventi.Impegno

import Applicazioni.Persistenza (Persistence (..))
import Applicazioni.Reactivegas (QS, TS, bianco, levelsEventi, maxLevel, sortEventi)
import Applicazioni.Sessione (Sessione (..))

import Lib.Console
import UI.Lib

wrapCostrActions ::
    (a -> Interfaccia ()) ->
    [MEnv (SUtente, TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String, Interfaccia ())]] ->
    [(String, Interfaccia ())]
wrapCostrActions g = concatMap (\f -> map (second (>> effetto)) $ f q g bocciato)
  where
    q = do
        s <- fst <$> statoSessione
        mu <- fmap fst <$> sel (readAccesso . snd)
        return (SUtente mu, s)

interrogazioni :: Interfaccia ()
interrogazioni =
    mano "interrogazioni sullo stato del gruppo" $
        ( wrapCostrActions P.output $
            [ costrQueryAnagrafe
            , costrQueryAccredito
            , costrQueryImpegni
            , costrQueryAssenso
            ]
        )

dichiarazioni k =
    onAccesso . const . mano "gestione dichiarazioni" $
        concat
            [ wrapCostrActions addEvento [costrEventiAccredito]
            , wrapCostrActions addEvento [costrEventiAcquisto]
            , wrapCostrActions addEvento [costrEventiImpegno]
            , wrapCostrActions addEvento [costrEventiAnagrafe, costrEventiResponsabili]
            , wrapCostrActions addEvento [costrEventiAssenso]
            ]
            ++ [ ("elimina delle dichiarazioni", eliminazioneEvento)
               , ("modifica il livello di considerazione delle ultime dichiarazioni", eventLevelSelector)
               , ("uscita", salvataggio >> k ())
               ]

baseloop :: Interfaccia ()
baseloop = rotonda $ \k -> do
    ms <- sel $ readState . fst
    case ms of
        Nothing -> P.errore $ ResponseOne "il gruppo non esiste ancora"
        Just s ->
            menu
                ("menu principale")
                [ ("uscita", salvataggio >> k ())
                , ("gestione dichiarazioni", dichiarazioni k)
                , ("effetto delle ultime dichiarazioni del gruppo", effetto)
                , ("scarica nuove chiavi da responsabile", creaChiavi)
                , ("digerisci tutte le dichiarazioni pubblicate (sincronizzatore)", sincronizza)
                ]

interfaccia :: MEnv ()
interfaccia = svolgi baseloop >>= interazione
