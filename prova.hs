import Control.Monad.State
import Data.IORef


g = unsafePerform $ newIORef 1

instance MonadState Int IO where
	get = 


