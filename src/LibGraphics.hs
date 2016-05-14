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
boardToPicture ((((x, y), 3) : xs), k) = (addCanMoveChecker x y) : (boardToPicture (xs, k))
boardToPicture _ = []

addBlackChecker :: Float -> Float -> Picture
addBlackChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/black.bmp") 

addCanMoveChecker :: Float -> Float -> Picture
addCanMoveChecker x y = Translate x y 
			$ Scale 0.3 0.3
			$ unsafePerformIO(loadBMP "data/canMove.bmp") 

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
handleEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) w = (move (x, y) (drawPosMove w))
handleEvents _ w = w



move :: Pos -> World -> World
move p (world, turn) = if (canDraw p world) then ((delX (goMove p world turn)), (turn `mod` 2)+1) 
							else (world, turn)

canDraw :: Pos -> [WorldObject] -> Bool
canDraw p ((p1, state) : xs) = if ((state == 3) && (areal p p1)) then True
								else (canDraw p xs)
canDraw p [] = False

onLine :: Pos -> [WorldObject] -> Int -> Bool--если сущ-ет checker с одинаковым цветом
onLine p ((p1, k) : xs) turn = if (horVertDiag p p1) && (k == turn)	then True
						else (onLine p xs turn)
onLine _ _ _ = False

horVertDiag :: Pos -> Pos -> Bool
horVertDiag (x, y) (x1, y1) = if( 
								((abs (x1-x)) < eps)  || 
								((abs $ y1-y) < eps)  || 
								((abs $ (abs $ x-x1)-(abs $ y-y1)) < eps)
								)then True
								else False
eps :: Float
eps = 10.0

isNear :: Pos -> [WorldObject] -> Bool--смотрим все 8 сторон, где должен существовать checker
isNear  (x, y) world = if(  (nearChecker (x-(offsetX), y-(offsetY)) world ) ||
							(nearChecker (x, y-(offsetY)) world ) ||
							(nearChecker (x+(offsetX), y-(offsetY)) world ) ||
							(nearChecker (x-(offsetX), y) world ) ||
							(nearChecker (x+(offsetX), y) world ) ||
							(nearChecker (x-(offsetX), y+(offsetY)) world ) ||
							(nearChecker (x, y+(offsetY)) world ) ||
							(nearChecker (x+(offsetX), y+(offsetY)) world )) then True
							else False

nearChecker :: Pos -> [WorldObject] -> Bool 
nearChecker p ((p1, k) : xs) = if (areal p p1) then 
									if k /= 0 then True
									else False
							   else (nearChecker p xs)
nearChecker _ _ = False 

notOut :: Pos -> [WorldObject] -> Bool -- сможем ли мы поставить этот элемент в наш Ворлд
notOut p ((p1, k) : xs) = if (areal p p1) then 
								if k /= 0 then False
								else True
							else (notOut p xs)
notOut _ _ = False 

goMove :: Pos -> [WorldObject] -> Int -> [WorldObject]
goMove p ((p1, k) : xs) turn = if ((areal p p1) && ( k == 3)) then ((p1, turn) : xs)
									else (p1, k) : (goMove p xs turn)
goMove _ [] _ = []

delX :: [WorldObject] -> [WorldObject]
delX ((p, state) : xs) = if (state == 3) then (p, 0) : (delX xs)
							else (p, state) : (delX xs)
delX [] = []

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