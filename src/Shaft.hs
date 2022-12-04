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
type Depth = Int

type Player = [Coord]
data PlatformType = NormalPlatform | SpikePlatform | LeftPlatform | RightPlatform deriving (Eq, Show)
data Tick = Tick
data Movement = Left | Right

data Mode = Easy | Medium | Hard deriving (Eq, Show)
data ModeMap = ModeMap
  {
    _easy   :: Modes,
    _medium :: Modes,
    _hard   :: Modes
  }
  deriving (Eq, Show)
type Frequency = [Int]
data Modes = Modes
  {
    _x                    :: [Int],
    _y                    :: [Int],
    _normalFrequency      :: Frequency,
    _spikeFrequency       :: Frequency,
    _leftFrequency           :: Frequency,
    _rightFrequency          :: Frequency
  }
  deriving (Eq, Show)
data LastDepth = LastDepth
  {
    _normal      :: Depth,
    _spike       :: Depth,
    _left        :: Depth,
    _right        :: Depth
  }
  deriving (Eq, Show)

data Game = Game {
  _player     :: Player,
  _platforms  :: SEQ.Seq (Platform, PlatformType),  
  _score      :: Score,
  _bestScore  :: Score,
  _health     :: Health,
  _alive      :: Bool,
  _modeMap    :: ModeMap,
  _mode       :: Mode,
  _lastPlatformDepth  :: LastDepth,
  _time       :: Int
} deriving (Show)

makeLenses ''Game
makeLenses ''ModeMap
makeLenses ''Modes
makeLenses ''LastDepth

gridWidth :: Int
gridWidth = 50
gridHeight :: Int
gridHeight = 20

initPlayer :: Player
-- initPlayer= [V2 (gridWidth `div` 2) (gridHeight - 3), V2 (gridWidth `div` 2) (gridHeight - 4)]
initPlayer= [V2 (gridWidth `div` 2) (gridHeight - 3)]

initlastDepth :: LastDepth
initlastDepth = LastDepth (-5) (-5) (-5) (-5)

initState :: Score -> IO Game
initState bestScore = do
  mode <- modeMaps
  return Game {
    _player     = initPlayer,
    _score      = 0,
    _platforms   = SEQ.empty,
    -- _platforms   =  singleton ([V2 0 5, V2 1 5, V2 2 5, V2 3 5, V2 4 5, V2 5 5], NormalPlatform),
    _bestScore  = bestScore,
    _health     = 10,
    _alive      = True,
    _modeMap          = mode,
    _mode             = Easy,
    _lastPlatformDepth      = initlastDepth,
    _time   = 0
  }

modeMaps :: IO ModeMap
modeMaps = do
  x    <- randomRs (0, gridWidth) <$> newStdGen
  y    <- randomRs (0, last initPlayer^._2) <$> newStdGen
  easyNormal <- randomRs (0, 5) <$> newStdGen
  easySpike <- randomRs (5, 10) <$> newStdGen
  easyLeft <- randomRs (8, 12) <$> newStdGen
  easyRight <- randomRs (8, 12) <$> newStdGen
  medium <- randomRs (3, 5) <$> newStdGen
  hard <- randomRs (0, 3) <$> newStdGen
  return $ ModeMap
    (Modes x y easyNormal easySpike easyLeft easyRight)
    (Modes x y medium medium medium medium)
    (Modes x y hard hard hard hard)

-- | Get game's relevant mode.
getModes :: Game -> Modes
getModes g = case g^.mode of
                Easy -> g^.modeMap.easy
                Medium -> g^.modeMap.medium
                Hard -> g^.modeMap.hard

setModes :: Modes -> Game -> Game
setModes m g = case g^.mode of
                  Easy -> g & modeMap.easy .~ m
                  Medium -> g & modeMap.medium .~ m
                  Hard -> g & modeMap.hard .~ m

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

inLeftPlatform :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
inLeftPlatform c bs = getAny $ foldMap (Any . inLeftPlatform' c) bs

inLeftPlatform' :: Coord -> (Platform, PlatformType) -> Bool
inLeftPlatform' c (b, LeftPlatform) = c `elem` b
inLeftPlatform' _ _ = False

inRightPlatform :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
inRightPlatform c bs = getAny $ foldMap (Any . inRightPlatform' c) bs

inRightPlatform' :: Coord -> (Platform, PlatformType) -> Bool
inRightPlatform' c (b, RightPlatform) = c `elem` b
inRightPlatform' _ _ = False

step  :: Game -> Game
step g = fromMaybe g $ do
  guard $ g^.alive
  return $ fromMaybe (step' g) (checkAlive g)
  -- return $ fromMaybe (step' g) (Just g)

step' :: Game -> Game
step' = incTime . createPlatforms . move . deletePlatformsLeft . deletePlatformsRight
-- Todo
-- step' = move

move :: Game -> Game
-- Todo
move = movePlatforms . movePlayer . onSpike . onLeft. onRight
-- move = movePlatforms

incTime :: Game -> Game
incTime g = g & time %~ (+1)

afterMoveSignleStep :: Game -> Game
afterMoveSignleStep g = fromMaybe g $ do 
                        guard (not $ isDead g) 
                        return g

movePlayerSingleStep :: Movement -> Game -> Game
movePlayerSingleStep Left g  = if shouldLeft g && g^.alive then afterMoveSignleStep (movePlayerHorizontally Left g) else g
movePlayerSingleStep Right g = if shouldRight g && g^.alive then afterMoveSignleStep (movePlayerHorizontally Right g)  else g

movePlayerHorizontally :: Movement -> Game -> Game
movePlayerHorizontally dir g =
  case dir of
    Left  -> if shouldLeft g then g & player %~ fmap (+ V2 (-1) 0) else g
    Right -> if shouldRight g then g & player %~ fmap (+ V2 1 0) else g

shouldLeft :: Game -> Bool
shouldLeft g = shouldLeft' [coord^._1 | coord <- g^.player]

shouldLeft' :: [Int] -> Bool
shouldLeft' xs = (xs /= []) && minimum xs > 0

shouldRight :: Game -> Bool
shouldRight g = shouldRight' [coord^._1 | coord <- g^.player]

shouldRight' :: [Int] -> Bool
shouldRight' xs = (xs /= []) && minimum xs < gridWidth - 1

movePlatforms :: Game -> Game
movePlatforms g = g & platforms %~ fmap movePlatform

movePlatform  :: (Platform, PlatformType) -> (Platform, PlatformType)
movePlatform  (plt, x) = (fmap (+ V2 0 1) plt, x)
-- movePlatform  (plt, SpikePlatform) = (fmap (+ V2 0 1) plt, SpikePlatform)
-- movePlatform  other = other

movePlayer :: Game -> Game
movePlayer g = let player' = g^.player
                   platforms' = g^.platforms
  in 
    if getAny $ foldMap (Any . flip isOnplatform platforms') player'
      then g & player %~ fmap (+ V2 0 (1))
      else incDepth (g & player %~ fmap (+ V2 0 (-1)))


isOnplatform :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
isOnplatform player platforms = getAny $ foldMap (Any . isOnplatform' player) platforms

isOnplatform' :: Coord -> (Platform, PlatformType) -> Bool
isOnplatform' player platform = player `elem` fst platform || (player + (V2 0 (-1))) `elem` fst platform

onSpike :: Game -> Game
onSpike g = let player' = g^.player
                platforms' = g^.platforms
                in 
                  if getAny $ foldMap (Any . flip isOnSpike platforms') player'
                    then g & health %~ (+(-1))
                    else g

isOnSpike :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
isOnSpike player platforms = getAny $ foldMap (Any . isOnSpike' player) platforms

isOnSpike' :: Coord -> (Platform, PlatformType) -> Bool
isOnSpike' player platform = (player `elem` fst platform || (player + (V2 0 (-1))) `elem` fst platform )&& ((snd platform) == SpikePlatform)

onLeft :: Game -> Game
onLeft g = let player' = g^.player
               platforms' = g^.platforms
               in 
                if getAny $ foldMap (Any . flip isOnLeft platforms') player'
                  then g & player %~ fmap (+ V2 (-1) 0)
                  else g

isOnLeft :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
isOnLeft player platforms = getAny $ foldMap (Any . isOnLeft' player) platforms

isOnLeft' :: Coord -> (Platform, PlatformType) -> Bool
isOnLeft' player platform = (player `elem` fst platform || (player + (V2 0 (-1))) `elem` fst platform) && ((snd platform) == LeftPlatform)

onRight :: Game -> Game
onRight g = let player' = g^.player
                platforms' = g^.platforms
                in 
                  if getAny $ foldMap (Any . flip isOnRight platforms') player'
                    then g & player %~ fmap (+ V2 1 0)
                    else g

isOnRight :: Coord -> SEQ.Seq (Platform, PlatformType) -> Bool
isOnRight player platforms = getAny $ foldMap (Any . isOnRight' player) platforms

isOnRight' :: Coord -> (Platform, PlatformType) -> Bool
isOnRight' player platform = (player `elem` fst platform || (player + (V2 0 (-1))) `elem` fst platform) && ((snd platform) == RightPlatform)



-- increase depth
incDepth :: Game -> Game
incDepth g = g & score %~ (+1)

checkAlive :: Game -> Maybe Game
checkAlive g = do
  guard $ isDead g
  return $ g & alive .~ False


isDead :: Game -> Bool
isDead g = let player' = g^.player
               platforms' = g^.platforms
               health' = g^.health
              in ((last player')^._2) < 0 || health' <= 0


addRandomPlatform :: PlatformType -> Game -> Game
addRandomPlatform NormalPlatform g = let (Modes (x:xs) (y:ys) (j:js) ms ls rs) = getModes g
                                         newModes = Modes xs ys js ms ls rs
                                         newObs = createPlatform NormalPlatform x
                                    in
                                      if g^.time - g^.lastPlatformDepth.normal >= j
                                      then setModes newModes g & platforms %~ (|> newObs) & ((lastPlatformDepth.normal) .~ g^.time)
                                      else g
addRandomPlatform SpikePlatform g = let (Modes (x:xs) (y:ys) js (m:ms) ls rs) = getModes g
                                        newModes = Modes xs ys js ms ls rs
                                        newObs = createPlatform SpikePlatform x
                                    in
                                      if g^.time - g^.lastPlatformDepth.spike >= m
                                      then setModes newModes g & platforms %~ (|> newObs) & ((lastPlatformDepth.spike) .~ g^.time)
                                      else g        
addRandomPlatform LeftPlatform g = let (Modes (x:xs) (y:ys) js ms (l:ls) rs) = getModes g
                                       newModes = Modes xs ys js ms ls rs
                                       newObs = createPlatform LeftPlatform x
                                    in
                                      if g^.time - g^.lastPlatformDepth.left >= l
                                      then setModes newModes g & platforms %~ (|> newObs) & ((lastPlatformDepth.left) .~ g^.time)
                                      else g      
addRandomPlatform RightPlatform g = let (Modes (x:xs) (y:ys) js ms ls (r:rs)) = getModes g
                                        newModes = Modes xs ys js ms ls rs
                                        newObs = createPlatform RightPlatform x
                                    in
                                      if g^.time - g^.lastPlatformDepth.right >= r
                                      then setModes newModes g & platforms %~ (|> newObs) & ((lastPlatformDepth.right) .~ g^.time)
                                      else g                                        
addRandomPlatform _ g = g


createPlatforms :: Game -> Game
createPlatforms g = addRandomPlatform NormalPlatform $ addRandomPlatform SpikePlatform $ addRandomPlatform LeftPlatform $ addRandomPlatform RightPlatform g

createPlatform :: PlatformType -> Int -> (Platform, PlatformType)
createPlatform pltType pos = (getPlatformCoord pltType pos, pltType)

-- getPlatform :: PlatformType -> Int -> Platform
-- getPlatform NormalPlatform  x = [V2 0 y, V2 1 y, V2 2 y]
-- getPlatform NormalPlatform  y = [V2 0 y, V2 1 y, V2 2 y, V2 3 y, V2 4 y, V2 5 y]

getPlatformCoord :: PlatformType -> Int -> Platform
getPlatformCoord _ x = [V2 x 0, V2 (x+1) 0, V2 (x+2) 0, V2 (x+3) 0, V2 (x+4) 0]
-- getPlatformCoord NormalPlatform x = [V2 x 0, V2 (x+1) 0, V2 (x+2) 0, V2 (x+3) 0, V2 (x+4) 0]
-- getPlatformCoord SpikePlatform x = [V2 x 0, V2 (x+1) 0, V2 (x+2) 0, V2 (x+3) 0, V2 (x+4) 0]


deletePlatformsLeft :: Game -> Game
deletePlatformsLeft g = case viewl $ g^.platforms of
                      EmptyL  -> g
                      a :< as -> if isOutOfBoundary a
                                  then deletePlatformsLeft (g & platforms .~ as)
                                  else g

deletePlatformsRight :: Game -> Game
deletePlatformsRight g = case viewr $ g^.platforms of
                      EmptyR  -> g
                      as :> a -> if isOutOfBoundary a
                                  then deletePlatformsRight (g & platforms .~ as)
                                  else g

-- | check the obastacle is out of boundray
isOutOfBoundary :: (Platform, PlatformType) -> Bool
-- isOutOfBoundary (coords, NormalPlatform) = (coords !! 2)^._2 > gridHeight
isOutOfBoundary (coords, _)  = last coords^._2 > gridHeight