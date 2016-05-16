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
		if(cntBlack + cntWhite == 64) then
			if(cntBlack == cntWhite) then 
				Pictures(
				(insertText (2*initLocation) (initLocation+12*(offsetX)) "DRAW") :
				boardToPicture (world, state, (cntBlack, cntWhite)) )
			else
				if(cntWhite > cntBlack) then
					Pictures(
					(insertText (2*initLocation) (initLocation+12*(offsetX)) "White wins") :
					boardToPicture (world, state, (cntBlack, cntWhite)) )
				else
					Pictures(
					(insertText (2*initLocation) (initLocation+12*(offsetX)) "Black wins") :
					boardToPicture (world, state, (cntBlack, cntWhite)) )
		else
			if(state == 1) then 
				Pictures(
				(insertText (2*initLocation) (initLocation+12*(offsetX)) "Black") :
				(insertTextNumb (2*initLocation + 9*(offsetX)) (initLocation+10*(offsetX)) "White" cntWhite) :
				(insertTextNumb (2*initLocation) (initLocation+10*(offsetX)) "Black" cntBlack) : 
				boardToPicture (world, state, (cntBlack, cntWhite)) )
			else
				Pictures(
				(insertText (2*initLocation) (initLocation+12*(offsetX)) "White") :
				(insertTextNumb (2*initLocation + 9*(offsetX)) (initLocation+10*(offsetX)) "White" cntWhite) :
				(insertTextNumb (2*initLocation) (initLocation+10*(offsetX)) "Black" cntBlack) : 
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
handleEvents _ w = (drawPosMove w)

move :: Pos -> World -> World
move p (world, turn, (cntB, cntW)) = 
	if (canDraw p world) then 
		if (turn == 2) then (del (changeTurn (reColorLine p ((goMove p world turn), turn, (cntB+1,cntW))) ((turn `mod` 2)+1))) 
		else  (del (changeTurn (reColorLine p ((goMove p world turn), turn, (cntB,cntW+1))) ((turn `mod` 2)+1))) 
	else 
		if(noMove world) then (world, (turn `mod` 2)+1, (cntB, cntW))
		else (world, turn, (cntB, cntW))

noMove :: [WorldObject] -> Bool -- проверяет, остались ли возможные ходы, если нет, то передаем ход
noMove ((_, state) : xs) = 
	if(state == 3) then False
	else (noMove xs)
noMove [] = True

changeTurn :: World -> Int -> World
changeTurn (x, _, p) turn = (x, turn, p)

canDraw :: Pos -> [WorldObject] -> Bool -- выбрал ли игрок, позицию куда можно ходить?
canDraw p ((p1, state) : xs) = if ((state == 3) && (areal p p1)) then True
								else (canDraw p xs)
canDraw _ [] = False

goMove :: Pos -> [WorldObject] -> Int -> [WorldObject] -- помечаем выбранную игроком позицию
goMove p ((p1, k) : xs) turn = if ((areal p p1) && ( k == 3)) then ((p1, turn) : xs)
									else (p1, k) : (goMove p xs turn)
goMove _ [] _ = []

del :: World -> World
del (w, turn, cnt) = ((delX w), turn, cnt)

delX :: [WorldObject] -> [WorldObject] -- удаляем выбранные позиции
delX ((p, state) : xs) = if (state == 3) then (p, 0) : (delX xs)
							else (p, state) : (delX xs)
delX [] = []

step :: Float -> World -> World
step _ w = w
{-bg <- loadBMP "./images/background3.bmp"-}
insertTextNumb :: Float -> Float -> String -> Int -> Picture
insertTextNumb x y w cnt 
	| w == "Black" =
		Translate x y
		$ Scale 0.3 0.3
		$ Color black
		$ Text (show cnt) -- Pictures( (unsafePerformIO(loadBMP "data/black.bmp")) : (Text (show cnt)) : [])
	| otherwise = 
		Translate x y
		$ Scale 0.3 0.3
		$ Color white
		$ Text (show cnt) -- Pictures( (unsafePerformIO(loadBMP "data/black.bmp")) : (Text (show cnt)) : [])
		
insertText :: Float -> Float -> String -> Picture
insertText x y w 
	| w == "Black" = 
		Translate x y
		$ Scale 0.4 0.4
		$ Color black
		$ Text $ w ++ "'s moving"
	| w == "Black wins" = 
		Translate x y
		$ Scale 0.4 0.4
		$ Color black
		$ Text $ w
	| w == "White wins" = 
		Translate x y
		$ Scale 0.4 0.4
		$ Color white
		$ Text $ w
	| otherwise = 
		Translate x y
		$ Scale 0.4 0.4
		$ Color white
		$ Text $ w ++ "'s moving"