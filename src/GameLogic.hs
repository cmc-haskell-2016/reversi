module GameLogic where

import Types

drawPosMove :: World -> World
drawPosMove (w, turn, cnt) = (reColor (canMove (eqState w turn) w turn) w, turn, cnt)

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
checkMove p w turn = concatMap (\pos -> f p turn w pos w)
  [ (-1, -1), (0, -1), (1, -1)
  , (-1,  0),          (1,  0)
  , (-1,  1), (0,  1), (1,  1) ]

f :: Pos -> Int -> [WorldObject] -> Pos -> [WorldObject] -> [Pos]
f (x, y) turn (((x1, y1), state) : xs) (kx, ky) w = case state of
  Player p | p /= turn && areal (x2, y2) (x1, y1)
    -> canReturn (x2, y2) w w turn (kx, ky)
  _ -> f (x, y) turn xs (kx, ky) w
  where
    (x2, y2) = (x + offsetX * kx, y + offsetY * ky)
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

-- | sdkfjhasdf
reColorLine :: Pos -> World-> World
reColorLine p (w, turn, cnt) = reColorCell p (noEmpty (findPos p w) turn w) (w, turn, cnt)

-- | sdfsdf
reColorCell :: Pos -> [([Pos],Pos)] -> World -> World
reColorCell p0 (p : ps) (w, state, cnt) = (reColorCell p0 ps  (countCNT ((drawLine p0 p (w, state)), state)))
reColorCell p0 [] (w, state, cnt) = (w,state,cnt)

countCNT :: ([WorldObject], Int) -> World -- пересчитывет кол-во черных и белых
countCNT (w, state) = (w, state, (changeCNT w (0, 0)))

changeCNT :: [WorldObject] -> CountBlackWhite -> CountBlackWhite
changeCNT ((x,state) : xs) (cntB, cntW) 
    | (state == 2) = (changeCNT xs (cntB, cntW+1))
    | (state == 1) = (changeCNT xs (cntB+1, cntW))
    | otherwise =  (changeCNT xs (cntB, cntW))
changeCNT [] cnt = cnt

{-changeColor :: Pos -> ([Pos], Pos) -> ([WorldObject], Int) -> [WorldObject] -- помечает точки, которые лежат p0 и p1,взависимости от turn
changeColor p0 (p : ps) (w,turn) = (changeColor p0 ps (drawLine p0 p w turn))
changeColor _ _ (w,_) = w -}

drawLine :: Pos -> ([Pos], Pos) -> ([WorldObject],Int) -> [WorldObject]
drawLine p0 ((p1:ps), k) ((x : xs),turn) = (drawCell p0 p1 k x turn) : (drawLine p0 ((p1:ps), k) (xs,turn))
drawLine _ _ ([],_) = []
drawLine _ ([],_) (w,turn) = w

drawCell :: Pos -> Pos -> Pos -> WorldObject -> Int -> WorldObject
drawCell (x0, y0) (x1, y1) (kx, ky) ((x, y), state) turn = 
    if(areal (x0, y0) (x1, y1)) then ((x, y), state)
    else
        if(areal (x0+kx*offsetX, y0+ky*offsetY) (x, y)) then ((x, y), turn)
        else (drawCell (x0+kx*offsetX, y0+ky*offsetY) (x1, y1) (kx, ky) ((x, y), state) turn)

noEmpty :: [Pos] -> Int -> [WorldObject] -> [([Pos], Pos)]
noEmpty (p:ps) turn w = 
     ((f2 p turn w (-1, -1) w) , (-1, -1)) : 
     ((f2 p turn w (0, -1) w) , (0, -1)) : 
     ((f2 p turn w (1, -1) w) , (1, -1)) : 
     ((f2 p turn w (-1, 0) w) , (-1, 0)) : 
     ((f2 p turn w (1, 0) w) , (1, 0)) : 
     ((f2 p turn w (-1, 1) w) , (-1, 1)) : 
     ((f2 p turn w (0, 1) w) , (0, 1)) : 
     ((f2 p turn w (1, 1) w) , (1, 1)) : []
noEmpty _ _ _ = []

f2 :: Pos -> Int -> [WorldObject] -> Pos -> [WorldObject] -> [Pos]
f2 (x0, y0) turn (((x1, y1), state) : xs) (kx, ky) w = 
    if (areal (x0+kx*(offsetX), y0+ky*(offsetY)) (x1, y1)) then
        if(turn == state) then (x1, y1):[]
        else
            if(state == 0 || state == 3) then []
            else (f2 (x0+kx*(offsetX), y0+ky*(offsetY)) turn w (kx, ky) w)
    else (f2 (x0, y0) turn xs (kx, ky) w)
f2 _ _ [] _ _ = [] 

findPos :: Pos -> [WorldObject] -> Maybe Pos -- находит точную точку
findPos p ((p1, state) : xs)
  | areal p p1 = Just p1
  | otherwise  = findPos p xs
findPos p _ = Nothing

isBetween :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBetween (a, b) (c, d) (i, j)
  | (a, b) > (c, d) = isBetween (c, d) (a, b) (i, j)
  | a == c          = a == x         && b < y && y < d    -- вертикаль
  | b == d          = b == y         && a < x && x < c    -- горизонталь
  | c - a == d - b  = c - x == d - y && a < x && x < c    -- диагональ вправо вверх
  | c - a == b - d  = c - x == y - d && c < x && x < a    -- диагональ вправо вниз
  | otherwise = False

isbtw :: Pos -> Pos -> WorldObject -> Bool -- лежит ли точка между p1 и p0
isbtw (x0, y0) (x1, y1) ((x, y), state) = 
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

horVertHor :: Pos -> Pos -> Bool -- проверяет, лежат ли 2 точки по горизонтали/вертикали/диагонали
horVertHor (x, y) (x1, y1) = if(((abs $ x-x1) < eps) || ((abs $ y - y1) < eps) || (abs ((abs $ y-y1)-(abs $ x-x1)) < eps) ) then True
                                else False

areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = ((abs (x1-x)) < 0.3*(offsetX) && ((abs (y1-y)) < 0.3*(offsetY)))

