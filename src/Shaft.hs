{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Shaft
  ( initGame
  , Game(..)
  , height
  , width
  , platform
  , health
  , score
  , dead
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
width = 20

type Coord = V2 Int

data Game = Game {
  -- _player :: 
  _score :: Int,
  _health :: Int,
  _dead :: Bool,
  _pause :: Bool,
  -- _platform :: [Coord]
  _platform :: Coord
} deriving (Show)


makeLenses ''Game


initGame :: IO Game
initGame = do 
  let g = Game {
      _score = 0
    , _dead = False
    , _health = 10
    , _platform = V2 10 10

  }
  return g