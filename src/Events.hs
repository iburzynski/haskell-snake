module Events where

import Control.Concurrent ( readChan, writeChan, Chan )
import Control.Monad.Reader
import Control.Monad.State ( MonadState(put, get), StateT )
import System.IO ( hSetEcho, stdin )
import System.Random ( randomIO )

import Data
import Render ( renderNext, renderOver )
import Util ( after )
import Data.Maybe (fromJust)

castTick :: Chan Event -> Time -> IO ()
castTick chan rate = forever $ do
  -- perpetually writes TickEvents to the channel queue at specified interval
  after rate $ writeChan chan TickEvent

castKey :: Chan Event -> IO ()
castKey chan = forever $ do
  -- hide console echo and wait for key input
  hSetEcho stdin False
  c <- getChar
  -- unhide console output
  hSetEcho stdin True
  -- add key event to the channel queue
  writeChan chan (KeyEvent c)

-- IO (State (Reader x))
play :: Chan Event -> Time -> ReaderT Config (StateT Game IO) ()
play chan rate = forever $ do
  -- pop event off channel queue and process
  event      <- liftIO $ readChan chan
  proceeding <- proceedGame event
  -- render console based on game status (proceeding or over)
  if proceeding then renderNext else renderOver

proceedGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => Event -> m Bool
-- processes events from the channel and returns whether the game is proceeding
proceedGame (KeyEvent k) = do
  case lookup k keys of
    -- if a valid direction key, turn the snake
    Just d  -> turnSnake d >> pure True
    Nothing -> pure True
  where
    keys = [('k', U), ('h', L), ('l', R), ('j', D)]
proceedGame TickEvent = do
  play <- canPlay
  if play
    then do
      eat <- canEat
      when eat eatFood
      moveSnake
      pure True
    else
      pure False

turnSnake :: MonadState Game m => Direction -> m ()
turnSnake d = do
  g <- get
  -- if requested dir. isn't opposite of current dir., update the dir.
  unless (isOpposite d $ getDirection g) $ put (g { getDirection = d })
  where
    opps = [(U, D), (L, R), (R, L), (D, U)]
    isOpposite d1 d2 = fromJust (lookup d1 opps) == d2

canPlay :: (MonadReader Config m, MonadState Game m) => m Bool
canPlay = do
  c <- ask
  g <- get
  let
  -- get max y and x coordinates
    (my, mx)          = getScreenSize c
  -- determine if snake's head is outside screen boundaries or intersecting tail
    (hp@(y, x) : tps) = getSnakeCoords . getSnake $ g
  pure . not $ (x < 0) || (x >= mx) || (y < 0) || (y >= my) || hp `elem` tps

canEat :: MonadState Game m => m Bool
canEat = do
  g <- get
  -- determine if snake head is at food position
  let
    sh = head . getSnakeCoords . getSnake $ g
    fp = getFoodCoords . getFood $ g
  pure $ sh == fp

eatFood :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
eatFood = do
  g      <- get
  config <- ask
  fcol   <- liftIO randomIO
  frow   <- liftIO randomIO
  let
    (scol, srow) = getScreenSize config
    oldFood  = getFoodCoords . getFood $ g
    oldSnake = getSnakeCoords . getSnake $ g
    newFood = (fcol `mod` scol, frow `mod` srow)
  put (g { getSnake = Snake (oldFood : oldSnake) -- extend length of snake
         , getFood  = Food newFood
         , getScore = getScore g + 1
         })

moveSnake :: MonadState Game m => m ()
moveSnake = do
  g <- get
  let
    s@(h : _) = getSnakeCoords . getSnake $ g
    d         = getDirection g
  put (g { getSnake = Snake (movePoint d h : init s) })
  where
    movePoint d (y, x) = case d of
        U -> (y - 1, x)
        D -> (y + 1, x)
        L -> (y, x - 1)
        R -> (y, x + 1)