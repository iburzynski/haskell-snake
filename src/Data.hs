module Data (
  Config (..),
  Direction (..),
  Event (..),
  Food (..),
  Game (..),
  Point,
  Score,
  Size,
  Snake (..),
  Time
) where

import Control.Monad.State
import Control.Monad.Reader

-- events to add to the channel queue:
data Event
  = TickEvent -- used to check if game is over, eat food and move the snake
  | KeyEvent Char -- used to handle keyboard input and change snake direction
  deriving Show

type Size       = (Int, Int)
type Time       = Int
data Config     = Config {
    getScreenSize   :: Size,
    getHeadChar     :: Char,
    getBodyChar     :: Char,
    getFoodChar     :: Char,
    getTickRate     :: Time,
    getBlinkRate    :: Time
  } deriving Show

type Point      = (Int, Int)
newtype Snake   = Snake { getSnakeCoords :: [Point] } deriving Show
newtype Food    = Food { getFoodCoords :: Point } deriving Show
type Score      = Int
data Direction  = U | L | R | D deriving (Show, Eq)
data Game       = Game {
    getSnake      :: Snake,
    getFood       :: Food,
    getDirection  :: Direction,
    getScore      :: Int
  } deriving Show
