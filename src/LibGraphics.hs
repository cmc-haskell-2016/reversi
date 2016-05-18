module LibGraphics  where
--my library
import Types
import GameLogic
import GameRecolorCells
--default library
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.IO.Unsafe
-- the game starts here
gameStart :: IO()
gameStart = play window background ping initWorld worldToPicture handleEvents step
    where
        window = InWindow "reversi" (800, 800) (250, 0)
        background = blue
        ping = 100
-- initial World
initWorld :: World
initWorld = (World (createBoard 7 initialLocation) (Player WhiteMove) (2,2))
-- create board
createBoard :: Int -> (Float, Float) -> [Cell]
createBoard n (x, y)
    | n > 0 = (createLine 8 x y) ++ (createBoard (n-1) (x, (y+cellHeight)) )
    | otherwise = (createLine 8 x y)
-- create a line of our World
createLine :: Int -> Float -> Float -> [Cell]
createLine n x y 
    | n > 0 = 
        if(inMidBlack (x0, y0)) then     (Cell (x0, y0) (Player BlackMove)) : (createLine (n-1) (x+cellWidth) y)
        else
            if(inMidWhite (x0, y0)) then (Cell (x0, y0) (Player WhiteMove)) : (createLine (n-1) (x+cellWidth) y)
            else                         (Cell (x0, y0) Empty) : (createLine (n-1) (x+cellWidth) y)
    | otherwise = []
    where (x0, y0) = pointToPos (x, y)
-- | преобразование внутреннего представления во внешнее
worldToPicture :: World -> Picture
worldToPicture (World world state (cntBlack, cntWhite)) 
    | cntBlack + cntWhite == 64 && (cntBlack == cntWhite) = 
        Pictures(
        (insertText (posToPoint (-3, 7)) "DRAW") :
        boardToPicture (World world state (cntBlack, cntWhite)))
    | (cntBlack + cntWhite == 64) && (cntBlack < cntWhite) = 
        Pictures(
        (insertText (posToPoint (0, 10)) "White wins") :
        boardToPicture (World world state (cntBlack, cntWhite)))
    | cntBlack + cntWhite == 64 && (cntBlack > cntWhite) = 
        Pictures(
        (insertText (posToPoint (0, 10)) "Black wins") :
        boardToPicture (World world state (cntBlack, cntWhite)))
    | otherwise = case state of
        Player WhiteMove ->
            Pictures(
            (insertText (posToPoint (-3, 7)) "White") :
            (insertTextNumb (posToPoint (-5,5)) "White" cntWhite) :
            (insertTextNumb (posToPoint (5,5)) "Black" cntBlack) : 
            boardToPicture (World world state (cntBlack, cntWhite)))
        _ ->
            Pictures(
            (insertText (posToPoint (-3, 7)) "Black") :
            (insertTextNumb (posToPoint (-5, 5)) "White" cntWhite) :
            (insertTextNumb (posToPoint (5, 5)) "Black" cntBlack) : 
            boardToPicture (World world state (cntBlack, cntWhite)))
--возвращает список картинок, в зависимости от State 
boardToPicture :: World -> [Picture]
boardToPicture (World ((Cell xy Empty) : xs) k cnt)              = (addEmptyChecker  (posToPoint xy)) : (boardToPicture (World xs k cnt))
boardToPicture (World ((Cell xy (Player BlackMove)) : xs) k cnt) = (addBlackChecker  (posToPoint xy)) : (boardToPicture (World xs k cnt))
boardToPicture (World ((Cell xy (Player WhiteMove)) : xs) k cnt) = (addWhiteChecker (posToPoint xy)) : (boardToPicture (World xs k cnt))
boardToPicture (World ((Cell xy PossibleMove) : xs) k cnt)       = (addPosChecker  (posToPoint xy)) : (boardToPicture (World xs k cnt))
boardToPicture _ = []
--"реагирование" на нажатую кнопку
handleEvents :: Event -> World -> World
handleEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) w =                -- | если нажата левая кнопка мыши
    move (pointToPos $ (x,y) - initialLocation - (2*cellWidth, 2*cellHeight))  w -- | учитываем начальное смещение, ПОЧЕМУ ИМЕННО ТАК-НЕ ЗНАЮ(метод проб и тычек)
handleEvents _ w = drawPosToMove w                                               -- | если ничего не нажато, рисуем возможные позиции для хода
-- | реагирование на нажатую левую кнопку мышки
move :: Pos -> World -> World
move p (World cells (Player WhiteMove) count) 
    | (checkSelectedPos p cells) =                                       -- | игрок выбрал возможный ход?
        (del (changeTurn (reColorLine p (World (markChoosedCell p cells (Player WhiteMove)) (Player WhiteMove) count)))) 
    | noMove cells = (changeTurn (World cells (Player WhiteMove) count)) -- | если не осталось возможных ходов, передаем ход черным
    | otherwise = (World cells (Player WhiteMove) count)                 -- | игрок выбрал позицию, куда нельзя ставить
move p (World cells (Player BlackMove) count) 
    | (checkSelectedPos p cells) =                                       -- | игрок выбрал возможный ход?
        (del (changeTurn (reColorLine p (World (markChoosedCell p cells (Player BlackMove)) (Player BlackMove) count)))) 
    | noMove cells = (changeTurn (World cells (Player BlackMove) count)) -- | если не осталось возможных ходов, передаем ход белым
    | otherwise = (World cells (Player BlackMove) count)                 -- | игрок выбрал позицию, куда нельзя ставить
move _ _ = undefined                                                     -- | такого не должно быть
-- проверяет, остались ли возможные ходы, если нет, то передаем ход
noMove :: [Cell] -> Bool 
noMove ((Cell _ PossibleMove) : _) = False
noMove (_:xs) = (noMove xs)
noMove [] = True
-- передаем ход игрока
changeTurn :: World -> World
changeTurn (World x (Player BlackMove) p) = (World x (Player WhiteMove) p)
changeTurn (World x (Player WhiteMove) p) = (World x (Player BlackMove) p)
changeTurn _ = undefined -- | that's impossible
-- выбрал ли игрок, позицию куда можно ходить?
checkSelectedPos :: Pos -> [Cell] -> Bool 
checkSelectedPos p ((Cell p1 state) : xs) 
    | (areal p p1)&&(isStatePossibleMove state) = True
    | otherwise = (checkSelectedPos p xs)
checkSelectedPos _ [] = False
-- помечаем, выбранную игроком, позицию
markChoosedCell :: Pos -> [Cell] -> State -> [Cell] 
markChoosedCell p ((Cell p1 state) : xs) turn 
    | ((areal p p1) && (isStatePossibleMove state)) = ((Cell p1 turn) : xs)
    | otherwise = (Cell p1 state) : (markChoosedCell p xs turn)
markChoosedCell _ [] _ = undefined -- | не должно, чтоьы он не нашел
-- замена позиции, помеченные "крестиком", на пустые
del :: World -> World
del (World w turn cnt) = (World (delX w) turn cnt)
-- заменяем "возможные ходы" на пустые позиции
delX :: [Cell] -> [Cell] 
delX ((Cell p state) : xs) 
    | (isStatePossibleMove state) = (Cell p Empty) : (delX xs)
    | otherwise = (Cell p state) : (delX xs)
delX [] = []
-- :D
step :: Float -> World -> World
step _ w = w
-- вставляем счетчик = кол-во белых/черных
insertTextNumb :: (Float, Float) -> String -> Int -> Picture
insertTextNumb (x, y) w cnt 
    | w == "Black" =
        Translate x y
        $ Scale 0.3 0.3
        $ Color black       -- | счетчик у черных - черного цвета
        $ Text (show cnt)   
    | otherwise = 
        Translate x y
        $ Scale 0.3 0.3
        $ Color white       -- | счетчик у белых - белого цвета
        $ Text (show cnt)
-- вставка текста
insertText :: (Float,Float) -> String -> Picture
insertText (x, y) w 
    | w == "Black" = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color black
        $ Text $ w ++ "'s moving" -- | Black's moving
    | w == "Black wins" = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color black
        $ Text $ w                -- | Black wins
    | w == "White wins" = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color white
        $ Text $ w                -- | White wins
    | otherwise = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color white
        $ Text $ w ++ "'s moving" -- | White's moving
-- инициализируем черные клетки в центр
inMidBlack :: (Int, Int) -> Bool
inMidBlack (x, y) = x == y && (x == 3+k || x == 4+k)
    where k = (fst (pointToPos initialLocation))
-- инициализируем белые клетки в центр
inMidWhite :: (Int, Int) -> Bool
inMidWhite (x, y) = (x+y==7+2*k) && (x == 3+k || x == 4+k)
    where k = (fst (pointToPos initialLocation))
-- return Black Picture
addBlackChecker :: (Float, Float) -> Picture
addBlackChecker (x, y) = Translate x y
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/black.bmp") 
-- return Possible Move Picture
addPosChecker :: (Float, Float) -> Picture
addPosChecker (x, y) = Translate x y
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- |почему 0,009? нужно для того, чтобы квадратики не слипались 
    $ unsafePerformIO(loadBMP "data/canMove.bmp") 
-- return White Picture
addWhiteChecker :: (Float, Float) -> Picture
addWhiteChecker (x, y) = Translate x y
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/white.bmp") 
-- Return Green Picture
addEmptyChecker :: (Float, Float) -> Picture
addEmptyChecker (x, y) = Translate x y
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/green.bmp")
