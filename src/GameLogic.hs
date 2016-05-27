module GameLogic where

import Types
-- помечает "крестиком" возможные позиции для хода
drawPosToMove :: World -> World
drawPosToMove (World w turn cnt prevW m) = (World (markCells (retPossibleMove (returnEqToTurn w turn) w turn) w) turn cnt prevW m)
-- обозначает "крестиком" позиции, куда можно ходить
markCells :: [Maybe Pos] -> [Cell] -> [Cell] 
markCells (p1 : xs) w 
    | p1 == Nothing = (markCells xs w)
    | otherwise = (markCells xs (switchCellState p1 w))
markCells [] w = w
-- change State, which is Empty to PossibleMove
switchCellState :: Maybe Pos -> [Cell] -> [Cell]
switchCellState (Just p) ((Cell p1 Empty) : xs) 
    | (areal p1 p) = ((Cell p1 PossibleMove) : xs)
    | otherwise = (Cell p1 Empty) : (switchCellState (Just p) xs)
switchCellState (Just p) ((Cell p1 state) : xs) 	= (Cell p1 state) : (switchCellState (Just p) xs)
switchCellState Nothing w = w
switchCellState _ [] = []
-- возвращает позиции клеток, куда можно ходить
retPossibleMove :: [Pos] -> [Cell] -> State -> [Maybe Pos] 
retPossibleMove (p1:p2) w turn = (retNeighbors p1 w turn) ++ (retPossibleMove p2 w turn)
retPossibleMove [] _ _ = []
-- смотрит всех соседей, возвращает, возможные клетки, которые пусты или помечены
retNeighbors :: Pos -> [Cell] -> State -> [Maybe Pos] 
retNeighbors p w turn = 
    {-
    (-1, 1)   (0, 1)     (1, 1)
    (-1, 0)      p       (1, 0)
    (-1, -1)  (0, -1)    (1, -1)-}
    (possibPos p turn w (-1, -1) w) :
    (possibPos p turn w (0, -1) w) :
    (possibPos p turn w (1, -1) w) :
    (possibPos p turn w (-1, 0) w) :
    (possibPos p turn w (1, 0) w) :
    (possibPos p turn w (-1, 1) w) :
    (possibPos p turn w (0, 1) w) :
    (possibPos p turn w (1, 1) w) :[]
-- ищет клетку с состояние /= состояниею игрока, если нашла идет в isExist
possibPos :: Pos -> State -> [Cell] -> Pos -> [Cell] -> Maybe Pos 
possibPos (x, y) (Player turn) ((Cell (x1, y1) (Player p)) : xs) (kx, ky) w 
    | areal  (x2, y2) (x1, y1) && (not (isEqMove p turn)) = 
        (isExist (x2, y2) w w (Player turn) (kx, ky))
    | otherwise = (possibPos (x, y) (Player turn) xs (kx, ky) w)
        where (x2, y2) = (x + kx, y + ky)
possibPos p0 (Player turn) (_ : xs) (kx, ky) w = (possibPos p0 (Player turn) xs (kx, ky) w)
possibPos _ _ [] _ _ = Nothing
possibPos _ _ _ _ _ = undefined
-- ищет клетку с состоянием: пусто или помечено, если нашла возвращает ее
isExist :: Pos -> [Cell] -> [Cell] -> State -> Pos -> Maybe Pos 
isExist (x, y) ((Cell (x1, y1) state) : xs) w (Player turn) (kx, ky)
    | areal (x, y) (x1, y1) = case state of
        Empty                            -> Just (x1, y1)
        PossibleMove                     -> Just (x1, y1)
        Player p | not (isEqMove p turn) -> (isExist (x+kx, y+ky) w w (Player turn) (kx, ky))
                 | otherwise             -> Nothing
    | otherwise = (isExist (x, y) xs w (Player turn) (kx, ky))
isExist _ [] _ _ _ = Nothing
isExist _ _ _ _ _ = undefined
 -- возвращает список тех клеток, состояние которых = состоянию ходящего игрока
returnEqToTurn :: [Cell] -> State -> [Pos]
returnEqToTurn ((Cell p (Player state)) : xs) (Player turn) 
    | isEqMove state turn = p : (returnEqToTurn xs (Player turn))
    | otherwise = (returnEqToTurn xs (Player turn))
returnEqToTurn (_:xs) (Player turn) = (returnEqToTurn xs (Player turn))
returnEqToTurn [] _ = []
returnEqToTurn _ _ = undefined
