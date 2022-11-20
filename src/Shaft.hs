module Shaft where

data Game = Game {} deriving (Show)

initGame :: IO Game
initGame = do 
  let g = Game {}
  return g