module GameLogic where

import Types

drawPosMove :: World -> World
drawPosMove (w, turn, cnt) = ((reColor (canMove (eqState w turn) w turn) w), turn, cnt)

canMove :: [Pos] -> [WorldObject] -> Int -> [Pos]
canMove (p1:p2) w turn = (checkMove p1 w turn) ++ (canMove p2 w turn)
canMove [] w turn = []

reColor :: [Pos] -> [WorldObject] -> [WorldObject]
reColor (p1 : xs) w = (reColor xs (eachCell p1 w))
reColor [] w = w

eachCell :: Pos -> [WorldObject] -> [WorldObject]
eachCell p ((p1, state) : xs) = if ((state == 0) && (areal p1 p)) then ((p1, 3) : xs)
								else (p1, state) : (eachCell p xs)
eachCell _ [] = []

checkMove :: Pos -> [WorldObject] -> Int -> [Pos]
checkMove p w turn = 
		(f p turn w (-1, -1)) ++
		(f p turn w (0, -1)) ++
		(f p turn w (1, -1)) ++
		(f p turn w (-1, 0)) ++
		(f p turn w (1, 0)) ++
		(f p turn w (-1, 1)) ++
		(f p turn w (0, 1)) ++
		(f p turn w (1, 1))

f :: Pos -> Int -> [WorldObject] -> Pos -> [Pos]
f (x, y) turn (((x1, y1), state) : xs) (kx, ky) = 
	if ((areal (x + (offsetX)*kx, y + (offsetY)*ky) (x1, y1)) && (state /= turn) && (state > 0) && (state < 3)) 
	then (x1 + (offsetX)*kx, y1 + (offsetY)*ky) : []
	else (f (x, y) turn xs (kx, ky))
f _ _ [] _  = []

eqState :: [WorldObject] -> Int -> [Pos]
eqState ((p, state) : xs) turn = if(state == turn) then p : (eqState xs turn)
									else (eqState xs turn)
eqState [] _ = []

areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = ((abs (x1-x)) < 0.3*(offsetX) && ((abs (y1-y)) < 0.3*(offsetY)))

reColorLine :: Pos -> World-> World
reColorLine p (w, turn, cnt) = (reColorCell p (onLine (findPos p w) w turn) (w,turn,cnt))

										
onLine :: [Pos] -> [WorldObject] -> Int -> [Pos]
onLine (p : ps) ((p1, state) : xs) turn = 
	if ((horVertHor p p1) && (turn == state)) then p1 : (onLine (p : ps) xs turn)
	else (onLine (p : ps) xs turn)
onLine _ _ _ = []

findPos :: Pos -> [WorldObject] -> [Pos]
findPos p ((p1, state) : xs) = 
	if (areal p p1) then p1:[]
	else (findPos p xs)
findPos p [] = []


reColorCell :: Pos -> [Pos] -> World -> World
reColorCell point (p : ps) (w,state,cnt) = (reColorCell point ps  (countCNT ((changeColor point p (w, state)), state)))
reColorCell point [] w = w

countCNT :: ([WorldObject], Int) -> World
countCNT (w, state) = (w, state, (changeCNT w (0, 0)))

changeCNT :: [WorldObject] -> CountBlackWhite -> CountBlackWhite
changeCNT ((x,state) : xs) (cntB, cntW) 
	| (state == 2) = (changeCNT xs (cntB, cntW+1))
	| (state == 1) = (changeCNT xs (cntB+1, cntW))
	| otherwise =  (changeCNT xs (cntB, cntW))
changeCNT [] cnt = cnt

changeColor :: Pos -> Pos -> ([WorldObject], Int) -> [WorldObject]
changeColor p0 p1 (((x,state) : xs), turn) = 
	if (isbtw p0 p1 (x,state)) then (x, turn) : (changeColor p0 p1 (xs,turn))
	else (x, state) : (changeColor p0 p1 (xs,turn))
changeColor _ _ ([], turn) = []

isbtw :: Pos -> Pos -> WorldObject -> Bool
isbtw (x0, y0) (x1, y1) ((x, y), state) | state /= 0 = 
	if (((abs $ x0 - x1) < eps) && ((abs $ x0-x) < eps)) then -- vertical
		if(y0 > y1) then 
			if(y0 > y && y > y1) then True
			else False
		else
			if(y1 > y && y > y0) then True
			else False
	else
		if(((abs $ y0-y1) < eps) && ((abs $ y0-y) < eps)) then -- horizontal
			if(x0 > x1) then
				if(x0 > x && x > x1) then True
				else False
			else
				if(x1 > x && x > x0) then True
				else False
		else -- diagonal
			if(((abs $ (abs $ x1 - x0) - (abs $ y1 - y0)) < eps) && 
				((abs $ (abs $ x - x0) - (abs $ y - y0)) < eps)) then
				if(y0 > y1) then
					if (x0 > x1) then
						if(x0 > x && x > x1) then True
						else False
					else 
						if (x1 > x && x > x0) then True
						else False
				else
					if(x1 > x0) then
						if(x1 > x && x > x0) then True
						else False
					else 
						if (x0 > x && x > x1) then True
						else False 
			else False		 
	|otherwise = False
horVertHor :: Pos -> Pos -> Bool
horVertHor (x, y) (x1, y1) = if((x == x1) || (y == y1) || ((abs $ y-y1) == (abs $ x-x1))) then True
								else False
