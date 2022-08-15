module Init (initConfig, initGame) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Maybe ( fromJust )
import System.Console.ANSI ( getTerminalSize )
import System.Random ( randomIO )

import Data

-- `MonadIO` is a typeclass which is compatible with every monad stack where IO occurs
initConfig :: MonadIO m => m Config
initConfig = do
  s <- liftIO getTerminalSize
  return $ Config
    {
      getScreenSize = fromJust s,
      getHeadChar   = 'o',
      getBodyChar   = 'x',
      getFoodChar   = '&',
      getTickRate   = 200,
      getBlinkRate  = 500
    }

initGame :: MonadIO m => m (Config, Game)
initGame = do
  -- generate pair of random integers for positioning food item
  fy     <- liftIO randomIO
  fx     <- liftIO randomIO
  config <- initConfig
  let
    -- get coordinates for screen size
    (sy, sx) = getScreenSize config
    -- determine initial food coordinates within screen size boundaries
    fp = (fy `mod` sy, fx `mod` sx)
    game = Game {
        getSnake     = Snake [(10, 12), (10, 11), (10, 10)] -- . reverse $ map ((,) 10) [ 10 .. 12 ],
      , getFood      = Food fp
      , getDirection = R
      , getScore     = 0
      }
  pure (config, game)
