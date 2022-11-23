module Main (main) where

-- import Lib
-- import UI(main)
import Shaft
import UI(gameInit)

main :: IO ()
main = do
    g <- gameInit
    return ()
