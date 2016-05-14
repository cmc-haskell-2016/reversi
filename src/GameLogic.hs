module GameLogic where

import Types

drawPosMove :: World -> World
drawPosMove (w, turn) = ((reColor (canMove (eqState w turn) w turn) w), turn)

canMove :: [Pos] -> [WorldObject] -> Int -> [Pos]
canMove (p1:p2) w turn = (checkMove p1 w turn) ++ (canMove p2 w turn)
canMove [] w turn = []
canMove _ w _ = []

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
f (x, y) turn (((x1, y1), state) : xs) (kx, ky) = if ((areal (x + (offsetX)*kx, y + (offsetY)*ky) (x1, y1)) && (state /= turn) && (state > 0) && (state < 3)) 
													then (x1 + (offsetX)*kx, y1 + (offsetY)*ky) : []
													else (f (x, y) turn xs (kx, ky))
f _ _ [] _  = []

eqState :: [WorldObject] -> Int -> [Pos]
eqState ((p, state) : xs) turn = if(state == turn) then p : (eqState xs turn)
									else (eqState xs turn)
eqState [] _ = []

areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = ((abs (x1-x)) < 0.3*(offsetX) && ((abs (y1-y)) < 0.3*(offsetY)))
