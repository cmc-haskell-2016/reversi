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
initWorld = ((createBoard 7 (initLocation) (initLocation)), 2, (2,2))

createBoard :: Int -> Float -> Float -> [WorldObject]
createBoard n x y | n > 0 = (createLine 8 x y) ++ (createBoard (n-1) x (y+(offsetY)))
				| otherwise = (createLine 8 x y)
createLine :: Int -> Float -> Float -> [WorldObject]
createLine n x y | n > 0 = 
	if ((x == initLocation+3*(offsetX) && y == initLocation+3*(offsetY)) || 
	(x == initLocation+4*(offsetY) && y == initLocation+4*(offsetY))) then ((x, y), 1) : (createLine (n-1) (x+(offsetX)) y) 
	else 
		if ((x == initLocation+3*(offsetX) && y == initLocation+4*(offsetY)) || 
		(x == initLocation+4*(offsetY) && y == initLocation+3*(offsetY))) then ((x, y), 2) : (createLine (n-1) (x+(offsetX)) y) 
		else ((x, y), 0) : (createLine (n-1) (x+(offsetX)) y)
				| otherwise = []

worldToPicture :: World -> Picture
worldToPicture (world, state, (cntBlack, cntWhite)) = 
	Pictures((insertText (2*initLocation) (initLocation+8*(offsetX)) "White" cntWhite) :
	(insertText (2*initLocation) (initLocation+10*(offsetX)) "Black" cntBlack) : 
	boardToPicture (world, state, (cntBlack, cntWhite)) )

boardToPicture :: World -> [Picture]
boardToPicture ((((x, y), 0) : xs), k,cnt) = (returnCell x y) : (boardToPicture (xs, k,cnt))
boardToPicture ((((x, y), 1) : xs), k,cnt) = (addBlackChecker x y) : (boardToPicture (xs, k,cnt))
boardToPicture ((((x, y), 2) : xs), k,cnt) = (addWhiteChecker x y) : (boardToPicture (xs, k,cnt))
boardToPicture ((((x, y), 3) : xs), k,cnt) = (addCanMoveChecker x y) : (boardToPicture (xs, k,cnt))
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
handleEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) (w,state,cnt) = (move (x, y) (drawPosMove (w, state, cnt)))
handleEvents _ w = w

move :: Pos -> World -> World
move p (world, turn, (cntB, cntW)) = 
	if (canDraw p world) then 
		if (turn == 2) then (del (changeTurn (reColorLine p ((goMove p world turn), turn, (cntB+1,cntW))) ((turn `mod` 2)+1)  )) 
		else  (del (changeTurn (reColorLine p ((goMove p world turn), turn, (cntB,cntW+1))) ((turn `mod` 2)+1)  )) 
	else (world, turn, (cntB, cntW))

changeTurn :: World -> Int -> World
changeTurn (x, _, p) turn = (x, turn, p)

canDraw :: Pos -> [WorldObject] -> Bool
canDraw p ((p1, state) : xs) = if ((state == 3) && (areal p p1)) then True
								else (canDraw p xs)
canDraw _ [] = False

goMove :: Pos -> [WorldObject] -> Int -> [WorldObject]
goMove p ((p1, k) : xs) turn = if ((areal p p1) && ( k == 3)) then ((p1, turn) : xs)
									else (p1, k) : (goMove p xs turn)
goMove _ [] _ = []

del :: World -> World
del (w, turn, cnt) = ((delX w), turn, cnt)

delX :: [WorldObject] -> [WorldObject]
delX ((p, state) : xs) = if (state == 3) then (p, 0) : (delX xs)
							else (p, state) : (delX xs)
delX [] = []

step :: Float -> World -> World
step _ w = w
{-bg <- loadBMP "./images/background3.bmp"-}
insertText :: Float -> Float -> String -> Int -> Picture
insertText x y w cnt =
		Translate x y
		$ Scale 0.1 0.1
		$ Color red
		$ Text (w ++ " " ++ (show cnt))