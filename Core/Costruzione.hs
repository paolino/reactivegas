{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A wrapper around Lib.Costruzione m to simplify interface construction
module Core.Costruzione (libero, output, toSupporto, password, scelte, upload, Supporto, runSupporto, CostrAction, download) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Trans (lift)

import Core.Types (Evento)
import qualified Lib.Passo as P -- (Costruzione , libero, upload, scelte, download,password)
import Lib.Response

import Debug.Trace

-- | Support monad for value construction with query value in reader and ability to fail
newtype Supporto m s b a = Supporto {unSupporto :: ReaderT (m s) (ExceptT String (P.Costruzione m b)) a}
    deriving
        ( Monad
        , Applicative
        , Functor
        , MonadError String
        , MonadCont
        )

instance (Monad m) => MonadReader s (Supporto m s b) where
    ask = Supporto $ ask >>= lift . lift . lift . lift
    local f (Supporto k) = Supporto $ local (fmap f) k

-- | Lifts construction into the support monad
toSupporto :: (Monad m) => P.Costruzione m b a -> Supporto m s b a
toSupporto = Supporto . lift . lift

-- | Given the query state and continuations for error/success, executes a Supporto action
runSupporto ::
    (Monad m) =>
    m s ->
    (String -> P.Costruzione m b c) ->
    (a -> P.Costruzione m b c) ->
    Supporto m s b a ->
    P.Costruzione m b c
runSupporto s kn kp (Supporto f) = runExceptT (runReaderT f s) >>= either kn kp

-- | Free step lifted to support monad
libero :: (Monad m, Read a) => Response -> Supporto m s b a
libero = toSupporto . P.libero

-- | Password step lifted to support monad
password :: (Monad m, Read a) => String -> Supporto m s b a
password = toSupporto . P.password

-- | Upload step lifted to support monad
upload :: (Read a, Monad m) => String -> Supporto m s b a
upload = toSupporto . P.upload

output t = toSupporto . P.output t

download :: (Show a, Monad m) => String -> String -> a -> Supporto m s b ()
download q f = toSupporto . P.download q f

-- | Choices step lifted to support monad
scelte :: (Monad m) => [(String, a)] -> Response -> Supporto m s b a
scelte xs = toSupporto . P.scelte xs

-- | Type for sets of constructive actions
type CostrAction m c q s =
    -- | read state
    m s ->
    -- | success action
    (q -> P.Costruzione m c ()) ->
    -- | failure action
    (String -> P.Costruzione m c ()) ->
    -- | list of constructive actions tagged with their selection name
    [(String, P.Costruzione m c ())]
