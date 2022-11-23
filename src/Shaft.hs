{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Shaft where

import Data.Monoid

import System.Random (Random(..), randomRs, newStdGen)
import Linear.V2 (V2(..))
import qualified Data.Sequence as SEQ
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Prelude hiding (Right, Left)
import Data.Sequence (ViewR(EmptyR, (:>)), viewr, (|>), ViewL (EmptyL, (:<)), viewl, singleton)
import Graphics.Vty.PictureToSpans (isOutOfBounds)
import Data.List (findIndex)
import GHC.IO.Handle.Types (Handle__(haDecoder))

type Name = ()
type Coord = V2 Int
type Score = Int
type Health = Int
type Platform = [Coord]


type Player = [Coord]
data PlatformType = NormalPlatform | SpikePlatform deriving (Eq, Show)
data Tick = Tick

data Game = Game {
  _player     :: Player,
  _platforms   :: SEQ.Seq (Platform, PlatformType),  
  _score      :: Score,
  _bestScore  :: Score,
  _health     :: Health
} deriving (Show)

makeLenses ''Game

gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 20

initPlayer :: Player
initPlayer= [V2 (gridWidth `div` 2) (gridHeight - 3), V2 (gridWidth `div` 2) (gridHeight - 4)]

initState :: Score -> IO Game
initState bestScore = do
  return Game {
    _player     = initPlayer,
    _score      = 0,
    -- _platforms   = SEQ.empty,
    _platforms   =  singleton ([V2 0 5, V2 1 5, V2 2 5, V2 3 5, V2 4 5, V2 5 5], NormalPlatform),
    _bestScore  = bestScore,
    _health     = 10
  }


step  :: Game -> Game
step g = fromMaybe g $ do
  -- guard $ g^.alive
  -- return $ fromMaybe (step' g) (checkAlive g)
  return $ fromMaybe (step' g) (Just g)

step' :: Game -> Game
step' = createPlatforms . move

move :: Game -> Game
-- move = movePlatforms . movePlayer
move = movePlatforms

movePlatforms :: Game -> Game
movePlatforms g = g & platforms %~ fmap movePlatform

movePlatform  :: (Platform, PlatformType) -> (Platform, PlatformType)
movePlatform  (plt, NormalPlatform) = (fmap (+ V2 0 1) plt, NormalPlatform)
movePlatform  other = other



inNormalPlatform :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
inNormalPlatform c bs = getAny $ foldMap (Any . inNormalPlatform' c) bs

inNormalPlatform' :: Coord -> (Platform, PlatformType) -> Bool
inNormalPlatform' c (b, NormalPlatform) = c `elem` b
inNormalPlatform' _ _ = False

inSpikePlatform :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
inSpikePlatform c bs = getAny $ foldMap (Any . inSpikePlatform' c) bs

inSpikePlatform' :: Coord -> (Platform, PlatformType) -> Bool
inSpikePlatform' c (b, SpikePlatform) = c `elem` b
inSpikePlatform' _ _ = False




createPlatforms :: Game -> Game
createPlatforms g = g & platforms %~ (|> (createPlatform NormalPlatform 5))

createPlatform :: PlatformType -> Int -> (Platform, PlatformType)
createPlatform pltType pos = (getPlatform pltType pos, pltType)

getPlatform :: PlatformType -> Int -> Platform
-- getPlatform NormalPlatform  x = [V2 0 y, V2 1 y, V2 2 y]
getPlatform NormalPlatform  y = [V2 0 y, V2 1 y, V2 2 y, V2 3 y, V2 4 y, V2 5 y]