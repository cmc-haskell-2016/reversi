module GameRecolorCells where

import Types
-- перекрашивание клеток                
reColorLine :: Pos -> World-> World
reColorLine p (World w turn cnt prevW m stg svg stplst) = (reColorCell p (cellViaDirection p turn w) (World w turn cnt prevW m stg svg stplst))
-- перекрашивание клеток, по 1 клетке проверяем
reColorCell :: Pos -> [(Maybe Pos,Pos)] -> World -> World
reColorCell p0 (p : ps) (World w state cnt prevW m stg svg stplst) = reColorCell p0 ps  (World (areCellsBetweenP0P1 p0 p w state) state cnt prevW m stg svg stplst)
reColorCell _ [] w = countCNT w
-- пересчитывет кол-во черных и белых
countCNT :: World -> World 
countCNT (World w state _ prevW m stg svg stplst) = (World w state (changeCNT w (0, 0)) prevW m stg svg stplst)
-- recalculate of total
changeCNT :: [Cell] -> CountBlackWhite -> CountBlackWhite
changeCNT ((Cell _ (Player p)) : xs) (cntBlack, cntWhite) = case p of 
    WhiteMove -> (changeCNT xs (cntBlack, cntWhite+1))
    BlackMove -> (changeCNT xs (cntBlack+1, cntWhite))
changeCNT (_ : xs) (cntBlack, cntWhite)  =  (changeCNT xs (cntBlack, cntWhite))
changeCNT [] cnt = cnt
-- change state of all cells, which are between p0 and p1
areCellsBetweenP0P1 :: Pos -> (Maybe Pos, Pos) -> [Cell] -> State -> [Cell]
areCellsBetweenP0P1 p0 (Just p1, k) (x : xs) turn = (isCellBetween p0 p1 k x turn) : (areCellsBetweenP0P1 p0 (Just p1, k) xs turn)
areCellsBetweenP0P1 _ (Nothing ,_) w _ = w 
areCellsBetweenP0P1 _ _ [] _ = []
-- идем из (x0, y0) в (x1, y1), если (x, y) встретилась меняем ее состояние
isCellBetween :: Pos -> Pos -> Pos -> Cell -> State -> Cell
isCellBetween (x0, y0) (x1, y1) (kx, ky) (Cell (x, y) state) turn  
    | (areal (x0, y0) (x1, y1)) = (Cell (x, y) state)                             -- если дошли из (x0, y0) в (x1, y1), тогда просто возвращаем клетку
    | (areal (x2, y2) (x, y)) = (Cell (x, y) turn)                                -- меняем состояние (х, у), если мы на (x, y)
    | otherwise =  (isCellBetween (x2, y2) (x1, y1) (kx, ky) (Cell (x, y) state) turn) -- переходим на след клетку, в зависимости от (kx, ky)
    where (x2, y2) = (x0+kx, y0+ky)
-- возвращаем список клеток c направлениями, состояние клеток = состоянию игрока, до которых можно добраться используя направление
cellViaDirection :: Pos -> State -> [Cell] -> [(Maybe Pos, Pos)]
cellViaDirection p turn w = 
    {-
    (-1, 1)   (0, 1)     (1, 1)
    (-1, 0)      p       (1, 0)
    (-1, -1)  (0, -1)    (1, -1)-}
     ((findClosestCell p turn w (-1, -1) w) , (-1, -1)) : 
     ((findClosestCell p turn w (0, -1) w) , (0, -1)) : 
     ((findClosestCell p turn w (1, -1) w) , (1, -1)) : 
     ((findClosestCell p turn w (-1, 0) w) , (-1, 0)) : 
     ((findClosestCell p turn w (1, 0) w) , (1, 0)) : 
     ((findClosestCell p turn w (-1, 1) w) , (-1, 1)) : 
     ((findClosestCell p turn w (0, 1) w) , (0, 1)) : 
     ((findClosestCell p turn w (1, 1) w) , (1, 1)) : []
-- находим клетку, состояние которой = состоянию игрока
findClosestCell :: Pos -> State -> [Cell] -> Pos -> [Cell] -> Maybe Pos
findClosestCell (x0, y0) (Player turn) ((Cell (x1, y1) state) : xs) (kx, ky) w 
    | (areal (x2, y2) (x1, y1)) = case state of                        -- | check (x2, y2) == (x1, y1) 
        Player WhiteMove -> 
            if(isEqMove WhiteMove turn ) then Just (x1, y1)            -- | если turn == WhiteMove -> return (x1, y1)
            else (findClosestCell (x2, y2) (Player turn) w (kx, ky) w) -- | переходим на другую клетку: (x0+kx, y0+ky)
        Player BlackMove ->                                            -- | если turn == BlackMove -> return (x1, y1)
            if(isEqMove BlackMove turn ) then Just (x1, y1)            -- | переходим на другую клетку: (x0+kx, y0+ky)
            else (findClosestCell (x2, y2) (Player turn) w (kx, ky) w)
        _ -> Nothing                                                   -- | пусто или помечено -> Nothing
    | otherwise = (findClosestCell (x0, y0) (Player turn) xs (kx, ky) w) -- | смотрим оставшиеся клетки
    where (x2, y2) = (x0+kx, y0+ky)
findClosestCell _ _ [] _ _ = Nothing -- | выход за пределы доски
findClosestCell _ _ _ _ _ = undefined