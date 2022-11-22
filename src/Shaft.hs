{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Shaft
  ( initGame
  , Game(..)
  , height
  , width
  , health
  , score
  , dead
  , platformWidth
  , player
  , normalPlatform
  , healingPlatform
  , spikePlatform
  , conveyorPlatform
  , temporaryPlatform
) where
-- module Shaft where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)


height :: Int
height = 20

width :: Int
width = 42

platformWidth :: Int
platformWidth = 8

type Coord = V2 Int
type Platform = [Coord]

data Game = Game {
  _player :: [Coord],
  _score :: Int,
  _health :: Int,
  _dead :: Bool,
  _pause :: Bool,
  _normalPlatform :: Platform,
  _healingPlatform :: Platform,
  _spikePlatform :: Platform,
  _conveyorPlatform :: Platform,
  _temporaryPlatform :: Platform
} deriving (Show)


makeLenses ''Game

-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do
  -- MaybeT $ guard . not <$>
  -- MaybeT . fmap Just $ 

initGame :: IO Game
initGame = do 
  let g = Game {
      _score = 0
    , _dead = False
    , _health = 10
    , _normalPlatform = [(V2 x (height `div` 2)) | x <- [(width `div` 2 - platformWidth `div` 2)..(width `div` 2 + platformWidth `div` 2)]]
    , _healingPlatform = [(V2 y 5) | y <- [7..(7+platformWidth)]]
    , _spikePlatform = [(V2 y 15) | y <- [12..(12+platformWidth)]]
    -- , _temporaryPlatform = ...
    -- , _conveyorPlatform = ...
    , _player = [(V2 (width `div` 2) (height `div` 2 + k)) | k <- [1,2]]
  }
  return g