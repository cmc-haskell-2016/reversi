module LibGraphics  where
--my library
import Types
import GameLogic
--default library
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

gameStart :: IO()
gameStart = play window background ping initWorld worldToPicture inputEvents step
{-
play 
:: Display	Display mode.
-> Color	Background color.
-> Int	Number of simulation steps to take for each second of real time.
-> world The initial world.
-> (world -> Picture) A function to convert the world a picture.
-> (Event -> world -> world) A function to handle input events.
-> (Float -> world -> world) A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
-> IO ()	 
-}
window :: Display 
window = InWindow "reversi" (1000, 800) (0, 0)

background :: Color
background = green

ping :: Int
ping = 10

initWorld :: World
initWorld = undefined

worldToPicture :: World -> Picture
worldToPicture = undefined

inputEvents :: Event -> World -> World
inputEvents = undefined

step :: Float -> World -> World
step = undefined
