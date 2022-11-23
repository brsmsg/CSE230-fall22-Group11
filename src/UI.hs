module UI where

import Shaft 
  ( Game,
    Tick(..),
    Score,
    Name,
    score,
    bestScore,
    platforms,
    player,
    gridHeight,
    gridWidth,
    initState,
    inNormalPlatform,
    inSpikePlatform
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

data Cell = Empty | Player | NormalPlatform | SpikePlatform

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: Game -> [Widget Name]
drawUI g = [C.center (padRight (Pad 2) (drawGrid g <+> drawStats g))]

drawStats :: Game -> Widget Name
-- drawStats g = hLimit 30 (vBox [drawGameOver (g^.alive), padTop (Pad 2) (drawScore (g^.score)), padTop (Pad 2) (drawBestScore (g^.bestScore))])
drawStats g = hLimit 30 (vBox [padTop (Pad 2) (drawScore (g^.score)), padTop (Pad 2) (drawBestScore (g^.bestScore))])

drawScore :: Score -> Widget Name
drawScore s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Current Score")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

drawBestScore :: Score -> Widget Name
drawBestScore n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Best Score ")
  $ C.hCenter
  $ padAll 1
  $ str (show n)

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
      | otherwise                           = Empty


drawCell :: Widget Name
drawCell Player = withAttr playerAttr space
drawCell Empty  = withAttr emptyAttr space
drawCell NormalPlatform = withAttr normalPlatformAttr space
drawCell SpikePlatform  = withAttr spikePlatformAttr upArrow

palyerAttr :: AttrName
playerAttr = attrName "playerAttr"

normalPlatformAttr :: AttrName
normalPlatformAttr = attrName "normalPlatformAttr"

spikePlatformAttr :: AttrName
spikePlatformAttr = attrName "spikePlatformAttr"

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"


drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr space


space :: Widget Name
space = str " "

upArrow :: Widget Name
upArrow = str "^"

leftArrow :: Widget Name
leftArrow = str "<"

rightArrow :: Widget Name
rightArrow = str ">"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (playerAttr, V.white `on` V.white)
  , (normalPlatformAttr, fg V.blue `V.withStyle` V.bold)
  , (spikePlatformAttr, fg V.yellow `V.withStyle` V.bold)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]