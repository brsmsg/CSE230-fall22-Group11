module UI where

import Shaft
import Brick
import Brick.BChan

import Control.Monad (forever, void)
import Control.Concurrent (forkIO)

import qualified Graphics.Vty as V
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (vBorder)

-- custom event fed into the app constantly
data Tick = Tick

-- not currently used, but will be easier to refactor
type Name = ()

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return 
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- Brick.BChan.newBChan 10
  forkIO $ forever $ do
    Brick.BChan.writeBChan chan Tick
  let vtyBuilder = V.mkVty V.defaultConfig
  initVty <- vtyBuilder
  g <- initGame
  void $ customMain initVty vtyBuilder (Just chan) app g


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continueWithoutRedraw g


drawUI :: Game -> [Widget Name]
drawUI g = [center (str"Left") <+> vBorder <+> center(str "Right")]

-- drawStats :: Game -> Widget Name
-- drawStats g = hLimit 20
--   $ vBox []

-- drawGrid :: Game -> Widget Name
-- drawGrid = undefined

theMap :: AttrMap
theMap = attrMap V.defAttr []
