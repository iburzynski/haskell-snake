module Util (
  after,
  setNoBuffering
) where

import Control.Concurrent
import Control.Monad.IO.Class
import System.IO

after :: MonadIO m => Int -> m a -> m a
after t m = liftIO (threadDelay (t * 1000)) >> m

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
