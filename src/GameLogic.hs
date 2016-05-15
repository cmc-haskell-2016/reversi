module GameLogic where

import Types

drawPosMove :: World -> World
drawPosMove (w, turn, cnt) = ((reColor (canMove (eqState w turn) w turn) w), turn, cnt)

reColor :: [Pos] -> [WorldObject] -> [WorldObject] -- обозначает "крестиком" позиции, куда можно ходить
reColor (p1 : xs) w = (reColor xs (eachCell p1 w))
reColor [] w = w

eachCell :: Pos -> [WorldObject] -> [WorldObject]
eachCell p ((p1, state) : xs) = if ((state == 0) && (areal p1 p)) then ((p1, 3) : xs)
								else (p1, state) : (eachCell p xs)
eachCell _ [] = []

canMove :: [Pos] -> [WorldObject] -> Int -> [Pos] -- возвращает позиции точек, куда можно ходить
canMove (p1:p2) w turn = (checkMove p1 w turn) ++ (canMove p2 w turn)
canMove [] w turn = []

checkMove :: Pos -> [WorldObject] -> Int -> [Pos] -- смотрит всех соседей
checkMove p w turn = 
		(f p turn w (-1, -1) w) ++
		(f p turn w (0, -1) w) ++
		(f p turn w (1, -1) w) ++
		(f p turn w (-1, 0) w) ++
		(f p turn w (1, 0) w) ++
		(f p turn w (-1, 1) w) ++
		(f p turn w (0, 1) w) ++
		(f p turn w (1, 1) w)

f :: Pos -> Int -> [WorldObject] -> Pos -> [WorldObject] -> [Pos] 
f (x, y) turn (((x1, y1), state) : xs) (kx, ky) w = 
	if ((areal (x + (offsetX)*kx, y + (offsetY)*ky) (x1, y1)) && (state /= turn) && (state > 0) && (state < 3)) 
	then (canReturn (x1 + (offsetX)*kx, y1 + (offsetY)*ky) w w turn (kx, ky))
	else (f (x, y) turn xs (kx, ky) w)
f _ _ [] _ _ = []

canReturn :: Pos -> [WorldObject] -> [WorldObject] -> Int -> Pos -> [Pos] -- 
canReturn (x, y) (((x1, y1), state) : xs) w turn (kx, ky) =
	if(areal (x, y) (x1, y1)) then
		if(state == 0 || state == 3) then (x1, y1):[]
		else 
			if (state /= turn) then (canReturn (x+kx*(offsetX), y+ky*(offsetY)) w w turn (kx, ky))
			else []
	else (canReturn (x, y) xs w turn (kx, ky))
canReturn _ [] _ _ _ = []

eqState :: [WorldObject] -> Int -> [Pos] -- возвращает список тех точек, у которых ход
eqState ((p, state) : xs) turn = if(state == turn) then p : (eqState xs turn)
									else (eqState xs turn)
eqState [] _ = []

-----------------------------------------------------
reColorLine :: Pos -> World-> World
reColorLine p (w, turn, cnt) = (reColorCell p (onLine (findPos p w) w turn) (w,turn,cnt))
										
reColorCell :: Pos -> [Pos] -> World -> World
reColorCell point (p : ps) (w,state,cnt) = (reColorCell point ps  (countCNT ((changeColor point p (w, state)), state)))
reColorCell point [] w = w

countCNT :: ([WorldObject], Int) -> World -- пересчитывет кол-во черных и белых
countCNT (w, state) = (w, state, (changeCNT w (0, 0)))

changeCNT :: [WorldObject] -> CountBlackWhite -> CountBlackWhite
changeCNT ((x,state) : xs) (cntB, cntW) 
	| (state == 2) = (changeCNT xs (cntB, cntW+1))
	| (state == 1) = (changeCNT xs (cntB+1, cntW))
	| otherwise =  (changeCNT xs (cntB, cntW))
changeCNT [] cnt = cnt

changeColor :: Pos -> Pos -> ([WorldObject], Int) -> [WorldObject] -- помечает точки, которые лежат p0 и p1,взависимости от turn
changeColor p0 p1 (((x,state) : xs), turn) = 
	if (isbtw p0 p1 (x,state)) then (x, turn) : (changeColor p0 p1 (xs,turn))
	else (x, state) : (changeColor p0 p1 (xs,turn))
changeColor _ _ ([], turn) = []

onLine :: [Pos] -> [WorldObject] -> Int -> [Pos] -- возвращает список из тех точек, до которых можно добраться по горизонтали/вертикали/диагонали
onLine (p : ps) ((p1, state) : xs) turn = 
	if ((horVertHor p p1) && (turn == state)) then p1 : (onLine (p : ps) xs turn)
	else (onLine (p : ps) xs turn)
onLine _ _ _ = []

findPos :: Pos -> [WorldObject] -> [Pos] -- находит точную точку
findPos p ((p1, state) : xs) = 
	if (areal p p1) then p1:[]
	else (findPos p xs)
findPos p [] = []

isbtw :: Pos -> Pos -> WorldObject -> Bool -- лежит ли точка между p1 и p0
isbtw (x0, y0) (x1, y1) ((x, y), state) | (state > 0)&&(state < 3) = 
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
			if(((abs ((abs $ x1 - x0) - (abs $ y1 - y0))) < eps) && 
				((abs ((abs $ x - x0) - (abs $ y - y0))) < eps)) then
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

horVertHor :: Pos -> Pos -> Bool -- проверяет, лежат ли 2 точки по горизонтали/вертикали/диагонали
horVertHor (x, y) (x1, y1) = if (((abs $ x-x1) < eps) || ((abs $ y - y1) < eps) || (abs ((abs $ y-y1)-(abs $ x-x1)) < eps) ) then True
								else False

areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = ((abs (x1-x)) < 0.3*(offsetX) && ((abs (y1-y)) < 0.3*(offsetY)))
