module Lib.Error
	where

import Control.Monad.Error (MonadError, throwError)

-- | eleva una computazione Maybe alla monad error , lanciando un errore in caso di Nothing
liftMaybe :: MonadError e m => Maybe a -> e -> m a
liftMaybe Nothing s = throwError s
liftMaybe (Just x) _ = return x 

-- | eleva una computazione Bool alla monad error , lanciando un errore in caso di False
liftBool :: MonadError e m => Bool -> e -> m ()
liftBool False e = throwError e
liftBool _ _ = return ()
