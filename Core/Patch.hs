{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Core.Patch
Description : Signed event patches and groups
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides types and functions for creating and verifying signed
patches (collections of events signed by a responsible user)
and groups (collections of patches signed by a group administrator).
-}
module Core.Patch
    ( Patch
    , firma
    , fromPatch
    , Group
    , fromGroup
    , firmante
    , Firmante (..)
    ) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Data.Maybe (fromJust)

import Core.Costruzione (Supporto, password)
import Core.Types (Esterno, Evento, Responsabile, Utente)
import Lib.Firmabile (Chiave, Firma, sign)

-- | A patch is a set of events signed by a responsible user
type Patch = (Chiave, Firma, [Evento])

-- | Extract the signature from a patch
firma :: Patch -> Firma
firma (_, x, _) = x

-- | Verify a patch is acceptable (known author, valid signature)
fromPatch
    :: (Show s, MonadReader s m, MonadError String m)
    => (s -> [Responsabile])
    -- ^ function to get responsible users from state
    -> Patch
    -- ^ patch to verify
    -> m [Esterno Utente]
    -- ^ events tagged with author
fromPatch getResponsibles (c, _f, xs) = do
    s <- ask
    let rs = getResponsibles s
    unless (c `elem` map (fst . snd) rs) $
        throwError "patch author is unknown"
    let u = fst . head . filter ((== c) . fst . snd) $ rs
    return $ map (u,) xs

-- | A group patch is a set of patches signed by a responsible user
type Group = (Chiave, Firma, [Patch])

-- | Extract events from a group patch with the signing user's name
fromGroup
    :: (Show s, MonadReader s m, MonadError String m, Functor m)
    => (s -> [Responsabile])
    -- ^ function to get responsible users from state
    -> Group
    -- ^ group to verify
    -> m (Utente, [Esterno Utente])
    -- ^ (signing user, all events)
fromGroup getResponsibles (c, _f, ps) = do
    s <- ask
    let rs = getResponsibles s
    unless (c `elem` map (fst . snd) rs) $
        throwError "group update author is unknown"
    let u = fst . head . filter ((== c) . fst . snd) $ rs
    events <- concat <$> mapM (fromPatch getResponsibles) ps
    return (u, events)

-- | Create a signer from a responsible user's credentials
firmante
    :: forall m s c.
       (Show s, Monad m)
    => Responsabile
    -> Supporto m s c (Firmante s)
firmante (u, (c, s)) = do
    p <- password $ u ++ ", your responsible password:"
    when (null p) $ throwError "incorrect password"
    case sign (s, p) (undefined :: (), undefined :: s) of
        Nothing -> throwError "incorrect password"
        Just _ ->
            return $
                Firmante $ \b ps ->
                    (c, fromJust $ sign (s, p) (ps, b), ps)

-- | Existential wrapper for a signing function
newtype Firmante b = Firmante
    (forall a. (Show a) => b -> [a] -> (Chiave, Firma, [a]))
