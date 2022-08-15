module Main where

import Control.Concurrent ( newChan, forkIO )
import Control.Monad.Reader ( void, ReaderT(runReaderT) )
import Control.Monad.State ( void, StateT(runStateT) )

import Data ( Config(getTickRate, getBlinkRate) )
import Events ( play, proceedGame, castTick, castKey )
import Init ( initGame )
import Util ( setNoBuffering )

main :: IO ()
main = do
  setNoBuffering
  -- create a channel queue for processing events
  chan        <- newChan
  (cfg, game) <- initGame
  let bRate = getBlinkRate cfg
  -- create thread to write TickEvents to channel queue
  forkIO $ castTick chan (getTickRate cfg)
  -- create thread to listen for key input and write KeyEvents to channel queue
  forkIO $ castKey chan
  void $ runStateT (runReaderT (play chan bRate) cfg) game
  --                                             ^ pass init. config to Reader
  --                                                  ^ pass init. game state to State

-- 1. `play` is applied to the channel and blink rate, returning a ReaderT value
--     play :: Chan Event -> Time -> ReaderT Config (StateT Game IO) ()
-- 2. The Reader function is extracted and applied to the config value, returning a StateT value
-- 3. The State function is extracted and applied to the game state value, returns IO (Game, ()) val
-- 4. `void` discards the result value