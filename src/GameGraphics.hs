module GameGraphics where

import Types
import GameLogic

import Graphics.Gloss

gameStart :: IO()
gameStart = display --play display bgColor numSimStep initWorld worldToPicture indivInputEvents

display :: Display 
display = InWindow "Reversi" (600, 800) (0, 0) 

bgColor :: Color 
bgColor = green

numSimStep :: Int 
numSimStep = 10

initWorld :: createWorld
initWorld = undefined

worldToPicture :: Object -> Picture
worldToPicture = undefined

indivInputEvents :: Event -> World -> World
indivInputEvents = undefined