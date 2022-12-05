module Controller where

import Shaft (Game (..), Tick(..), Name(..), step, initState, bestScore, movePlayerSingleStep, Movement (Left, Right), shouldRight, shouldLeft)
import Brick ( continue, halt, EventM, BrickEvent(VtyEvent, AppEvent), Next)
import qualified Graphics.Vty as V
import Control.Lens ((^.), (&), (.~), (%~))
import Prelude hiding (Right, Left)
import Linear (V2(V2))
import Control.Monad.IO.Class (liftIO)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                        = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))        = continue $ movePlayerSingleStep Left g
handleEvent g (VtyEvent (V.EvKey V.KRight []))       = continue $ movePlayerSingleStep Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') []))  = liftIO (initState (g^.bestScore)) >>= continue
handleEvent g _ = continue g
