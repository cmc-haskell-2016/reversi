module LibGraphics  where
--my library
import Types
import GameLogic
--default library
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe
import Codec.BMP

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
window = InWindow "reversi" (800, 800) (400, 100)

background :: Color
background = blue

ping :: Int
ping = 10

initWorld :: World
initWorld = createWorld

worldToPicture :: World -> Picture
worldToPicture w = Pictures ((insertText "Black"):(createBoard 7 initLocation))

createBoard :: Int -> Float -> [Picture]
createBoard n k | n > 0 = (createLine 8 initLocation k) ++ (createBoard (n-1) (k+offsetY))
			| otherwise = (createLine 8 initLocation k) ++ (addCheckers (initLocation+3*(offsetX)) (initLocation+3*(offsetY)))

addCheckers :: Float -> Float -> [Picture]
addCheckers x y = (addBlackChecker x y) :(addWhiteChecker x (y+(offsetY))) :(addWhiteChecker (x+(offsetX)) y) : (addBlackChecker (x+(offsetX)) (y+(offsetY))) :[]

addBlackChecker :: Float -> Float -> Picture
addBlackChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/black.bmp") 

addWhiteChecker :: Float -> Float -> Picture
addWhiteChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/white.bmp") 

createLine :: Int -> Float -> Float -> [Picture]
createLine n x y | n > 0 = (returnCell x y): (createLine (n-1) (x+offsetX) y)
			| otherwise = []

returnCell :: Float -> Float ->Picture
returnCell x y = 
			Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/green.bmp")

createCell :: Float -> [Picture]
createCell n| n > 0 = (returnCell (n) (n*3)) : (returnCell n n) : (insertText "I'm here"):[] 			
			| otherwise = []

insertText :: String -> Picture
insertText w =
		Translate (-100.0) (0.0)
		$ Scale 0.1 0.1
		$ Color red
		$ Text w


inputEvents :: Event -> World -> World
inputEvents  _ w = w

step :: Float -> World -> World
step _ w = w
{-bg <- loadBMP "./images/background3.bmp"
-}