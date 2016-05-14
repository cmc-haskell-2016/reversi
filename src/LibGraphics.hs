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
gameStart = play window background ping initWorld worldToPicture handleEvents step
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
ping = 1

initWorld :: World
initWorld = ((createBoard 7 (initLocation) (initLocation)), 2)

createBoard :: Int -> Float -> Float -> [WorldObject]
createBoard n x y | n > 0 = (createLine 8 x y) ++ (createBoard (n-1) x (y+(offsetY)))
				| otherwise = (createLine 8 x y)
createLine :: Int -> Float -> Float -> [WorldObject]
createLine n x y | n > 0 = if ((x == initLocation+3*(offsetX) && y == initLocation+3*(offsetY)) || 
								(x == initLocation+4*(offsetY) && y == initLocation+4*(offsetY))) then ((x, y), 1) : (createLine (n-1) (x+(offsetX)) y) 
							else 
								if ((x == initLocation+3*(offsetX) && y == initLocation+4*(offsetY)) || 
								(x == initLocation+4*(offsetY) && y == initLocation+3*(offsetY))) then ((x, y), 2) : (createLine (n-1) (x+(offsetX)) y) 
								else ((x, y), 0) : (createLine (n-1) (x+(offsetX)) y)
				| otherwise = []

worldToPicture :: World -> Picture
worldToPicture world = Pictures(boardToPicture world)

boardToPicture :: World -> [Picture]
boardToPicture ((((x, y), 0) : xs), k) = (returnCell x y) : (boardToPicture (xs, k))
boardToPicture ((((x, y), 1) : xs), k) = (addBlackChecker x y) : (boardToPicture (xs, k))
boardToPicture ((((x, y), 2) : xs), k) = (addWhiteChecker x y) : (boardToPicture (xs, k))
boardToPicture _ = []

addBlackChecker :: Float -> Float -> Picture
addBlackChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/black.bmp") 

addWhiteChecker :: Float -> Float -> Picture
addWhiteChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/white.bmp") 

returnCell :: Float -> Float -> Picture
returnCell x y = 
			Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/green.bmp")

handleEvents :: Event -> World -> World
handleEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) w = (move (x, y) w)
handleEvents _ w = w

move :: Pos -> World -> World
move p (world, turn) = if(willTurn p world) then ((goMove p world turn), ((turn `mod` 2)+1)) 
							else (world, turn)
move _ w = w

willTurn :: Pos -> [WorldObject] -> Bool
willTurn p ((p1, k) : xs) = if (areal p p1) then 
								if k /= 0 then False
								else True
							else (willTurn p xs)
willTurn _ _ = False 

goMove :: Pos -> [WorldObject] -> Int -> [WorldObject]
goMove p ((p1, k) : xs) turn | k == 0 = if (areal p p1) then ((p1, turn) : xs)
									else (p1, k) : (goMove p xs turn)
						| k /= 0 = (p1, k) : (goMove p xs turn)
						| otherwise = []
goMove _ w _ = w
areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = ((abs (x1-x)) < 0.3*(offsetX) && ((abs (y1-y)) < 0.3*(offsetY)))

step :: Float -> World -> World
step _ w = w
{-bg <- loadBMP "./images/background3.bmp"
insertText :: String -> Picture
insertText w =
		Translate (2*initLocation) (initLocation+10*(offsetX))
		$ Scale 0.1 0.1
		$ Color red
		$ Text w
-}