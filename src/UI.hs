module UI where

import Shaft 
  ( Game,
    Tick(..),
    Score,
    Name,
    Mode,
    mode,
    score,
    bestScore,
    health,
    alive,
    platforms,
    player,
    gridHeight,
    gridWidth,
    initState,
    inNormalPlatform,
    inSpikePlatform,
    inLeftPlatform,
    inHealPlatform,
    inRightPlatform
  )

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.BChan as BChan
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Lens.Micro ((^.), mapped)
import Linear.V2 (V2(..))
import Control.Monad ( forever )
import Brick.Util ( fg, on )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import GHC.IO (unsafePerformIO)
import Controller(handleEvent)

import Brick
  ( App(..), BrickEvent(..), Padding(..), EventM, Next, Widget, AttrName, AttrMap,
    neverShowCursor, customMain, attrMap, hLimit, vBox, withBorderStyle, str, withAttr, 
    continue, halt, emptyWidget, padRight, padLeft, padTop, padAll, padBottom, hBox, attrName, (<+>)
  )

data Cell = Empty | Player | NormalPlatform | SpikePlatform | LeftPlatform | RightPlatform | HealPlatform

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawStats g <+>  drawGrid g))]

drawStats :: Game -> Widget Name
drawStats g = hLimit 30 (vBox [padTop (Pad 2) (drawMode (g^.mode)), padTop (Pad 2) (drawScore (g^.score)), padTop (Pad 2) (drawBestScore (g^.bestScore)), padTop (Pad 2) (drawHealth (g^.health)), drawGameOver (g^.alive)])

drawScore :: Score -> Widget Name
drawScore s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Current Score")
  $ C.hCenter
  $ padAll 1
  $ str (show s)

drawBestScore :: Score -> Widget Name
drawBestScore s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Best Score ")
  $ C.hCenter
  $ padAll 1
  $ str (show s)

drawHealth :: Score -> Widget Name
drawHealth s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Health ")
  $ C.hCenter
  $ padAll 1
  $ str (show s)

drawMode :: Mode -> Widget Name
drawMode s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Mode ")
  $ C.hCenter
  $ padAll 1
  $ str (show s)

drawGameOver :: Bool -> Widget Name
drawGameOver False = withAttr gameOverAttr $ C.hCenter $ str "Game Over"
drawGameOver _    = emptyWidget

gameInit :: IO Game
gameInit = 
  do
    channel <- BChan.newBChan 10
    forkIO $ forever $ do
      BChan.writeBChan channel Tick
      threadDelay 400000
    state <- initState 0
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    customMain initialVty builder (Just channel) app state


drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeRounded 
  $ B.borderWithLabel (str " NS-SHAFT ")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [gridHeight - 1,gridHeight - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..gridWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem`(g^.player)                 = Player
      | inNormalPlatform c (g ^. platforms) = NormalPlatform
      | inSpikePlatform c (g ^. platforms)  = SpikePlatform
      | inLeftPlatform c (g ^. platforms)   = LeftPlatform
      | inRightPlatform c (g ^. platforms)  = RightPlatform
      | inHealPlatform c (g ^. platforms)   = HealPlatform
      | otherwise                           = Empty


drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr space
drawCell Empty  = withAttr emptyAttr space
drawCell NormalPlatform = withAttr normalPlatformAttr underscore
drawCell SpikePlatform  = withAttr spikePlatformAttr upArrow
drawCell LeftPlatform = withAttr leftPlatformAttr leftArrow
drawCell RightPlatform = withAttr rightPlatformAttr rightArrow
drawCell HealPlatform = withAttr healPlatformAttr plus

playerAttr :: AttrName
playerAttr = attrName "playerAttr"

normalPlatformAttr :: AttrName
normalPlatformAttr = attrName "normalPlatformAttr"

spikePlatformAttr :: AttrName
spikePlatformAttr = attrName "spikePlatformAttr"

leftPlatformAttr :: AttrName
leftPlatformAttr = attrName "leftPlatformAttr"

rightPlatformAttr :: AttrName
rightPlatformAttr = attrName "rightPlatformAttr"

healPlatformAttr :: AttrName
healPlatformAttr = attrName "healPlatformAttr"

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

space :: Widget Name
space = str " "

underscore :: Widget Name
underscore = str "*"

upArrow :: Widget Name
upArrow = str "^"

leftArrow :: Widget Name
leftArrow = str "<"

rightArrow :: Widget Name
rightArrow = str ">"

plus :: Widget Name
plus = str "+"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (playerAttr, V.white `on` V.white)
  , (normalPlatformAttr, fg V.white `V.withStyle` V.bold)
  , (spikePlatformAttr, fg V.yellow `V.withStyle` V.bold)
  , (leftPlatformAttr, fg V.blue `V.withStyle` V.bold)
  , (rightPlatformAttr, fg V.red `V.withStyle` V.bold)
  , (healPlatformAttr, fg V.green `V.withStyle` V.bold)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]