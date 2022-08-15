module Render (
  renderOver,
  renderNext
) where

import Control.Monad.Reader ( forever, MonadIO(..), MonadReader(ask) )
import Control.Monad.State  ( MonadState(get) )
import System.Console.ANSI  ( clearScreen, setCursorPosition )

import Data
import Util ( after )

renderNext :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderNext = do
  config <- ask
  g <- get
  let (y, x) = getScreenSize config
  liftIO clearScreen
  liftIO $ renderSnake (getHeadChar config) (getBodyChar config) (getSnake g)
  liftIO $ renderCharacter (getFoodChar config) (getFoodCoords . getFood $ g)
  liftIO $ setCursorPosition (y + 2) (x - 7)
  liftIO . putStrLn $ "Score " ++ (show . getScore $ g)

renderCharacter :: Char -> Point -> IO ()
renderCharacter c (y, x) = do
  setCursorPosition y x
  putChar c
  pure ()

renderSnake :: Char -> Char -> Snake -> IO ()
renderSnake c d (Snake (hp : tps)) = do
  renderCharacter c hp
  mapM_ (renderCharacter d) tps

renderOver :: (MonadIO m, MonadReader Config m) => m ()
renderOver = do
  c <- ask
  let (row, col) = getScreenSize c
  forever $ do
    after (getBlinkRate c) $ liftIO clearScreen
    after (getBlinkRate c) $ do
      liftIO $ setCursorPosition (div row 2) (div col 2 - 7)
      liftIO $ putStr "Game Over"