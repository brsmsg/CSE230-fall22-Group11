module UI where

import Shaft 
import Brick
-- import Brick.BChan

import Control.Monad (forever, void)
import Control.Concurrent (forkIO)

import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (vBorder)

import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
-- custom event fed into the app constantly
data Tick = Tick

-- not currently used, but will be easier to refactor
type Name = ()

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

data Cell 
  = Player 
  | NormalPlatform 
  | HealingPlatform
  | SpikePlatform
  | TemporaryPlatform
  | ConveyorPlatform
  | Empty

main :: IO ()
main = do
  chan <- Brick.BChan.newBChan 10
  forkIO $ forever $ do
    Brick.BChan.writeBChan chan Tick
  let vtyBuilder = V.mkVty V.defaultConfig
  initVty <- vtyBuilder
  g <- initGame
  void $ customMain initVty vtyBuilder (Just chan) app g


handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = continueWithoutRedraw 

drawUI :: Game -> [Widget Name]
-- drawUI g = [center (str"Left") <+> vBorder <+> center(str "Right")]
drawUI g = [ C.center $ padTop (Pad 2) (drawStats g) <=> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 44
  $ hBox [ drawScore (g ^. score)
        --  , padTop (Pad 2) $ drawGameOver (g ^. dead)
         , drawHealth (g ^. health)
         ]

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget



drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "NS-SHAFT")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height - 1, height - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width - 1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. player = Player
      | c `elem` g ^. normalPlatform = NormalPlatform
      | c `elem` g ^. healingPlatform = HealingPlatform
      | c `elem` g ^. spikePlatform = SpikePlatform
      | otherwise = Empty



drawCell :: Cell -> Widget Name
drawCell NormalPlatform = withAttr normalPlatformAttr cw
drawCell HealingPlatform = withAttr healingPlatformAttr cw
drawCell SpikePlatform  = withAttr spikePlatformAttr cw
drawCell TemporaryPlatform  = withAttr temporaryPlatformAttr cw
drawCell ConveyorPlatform  = withAttr conveyorPlatformAttr cw
drawCell Empty          = withAttr emptyAttr cw
drawCell Player         = withAttr playerAttr cw

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawHealth :: Int -> Widget Name
drawHealth n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Health")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

cw :: Widget Name
cw = str " "


theMap :: AttrMap
theMap = attrMap V.defAttr [
  (playerAttr, V.red `on` V.red),
  (normalPlatformAttr, V.white `on` V.white),
  (spikePlatformAttr, V.blue `on` V.blue),
  (healingPlatformAttr, V.green `on` V.green),
  (temporaryPlatformAttr, V.yellow `on` V.yellow),
  (conveyorPlatformAttr, V.magenta `on` V.magenta)
  ]

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

normalPlatformAttr :: AttrName
normalPlatformAttr = attrName "normalPlatformAttr"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

playerAttr :: AttrName
playerAttr = attrName "playerAttr"

spikePlatformAttr :: AttrName
spikePlatformAttr = attrName "spikePlatformAttr"

healingPlatformAttr :: AttrName
healingPlatformAttr = attrName "healingPlatformAttr"

temporaryPlatformAttr :: AttrName
temporaryPlatformAttr = attrName "temporaryPlatformAttr"

conveyorPlatformAttr :: AttrName
conveyorPlatformAttr = attrName "conveyorPlatformAttr"