module Applicazioni.Filesystem where

import Lib.Valuedfile

import Control.Monad (when, liftM2, forever,mplus)
import Control.Monad.Writer (runWriter, Writer)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***),second, first)
import Control.Exception (tryJust, SomeException (..))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,dupTChan,retry,atomically)

import System.Random (getStdGen,randomRs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), addExtension)

import Text.XHtml hiding ((</>),value)

import Lib.Valuedfiles  (Valuedfile (..), maybeParse, getValuedfiles, keepSpec

