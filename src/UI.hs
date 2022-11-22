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

data Cell = Player | NormalPlatform | Empty

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
handleEvent _ = continueWithoutRedraw 

drawUI :: Game -> [Widget Name]
-- drawUI g = [center (str"Left") <+> vBorder <+> center(str "Right")]
drawUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
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
      -- | c `elem` g ^. 
      | c == g ^. platform = NormalPlatform
      | otherwise = Empty



drawCell :: Cell -> Widget Name
drawCell NormalPlatform = withAttr normalPlatformAttr cw
drawCell Empty          = withAttr emptyAttr cw


drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

cw :: Widget Name
cw = str " "


theMap :: AttrMap
theMap = attrMap V.defAttr [(normalPlatformAttr, V.red `on` V.red)]

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

normalPlatformAttr :: AttrName
normalPlatformAttr = attrName "normalPlatformAttr"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"
