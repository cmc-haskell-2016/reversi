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
initWorld = (World (createBoard 7 initialLocation) (Player WhiteMove) (2,2) Nothing (-120, -120) Menu (Nothing, 0) ([], 0) 0)
-- create board
createBoard :: Int -> (Float, Float) -> [Cell]
createBoard n (x, y)
    | n > 0 = (createLine 8 x y) ++ (createBoard (n-1) (x, (y+cellHeight)) )
    | otherwise = (createLine 8 x y)
-- create a line of our World
createLine :: Int -> Float -> Float -> [Cell]
createLine n x y 
    | n > 0 = 
        if(inMidBlack (x0, y0)) then     -- инициализация в центре черных клеток
            (Cell (x0, y0) (Player BlackMove)) : (createLine (n-1) (x+cellWidth) y)
        else
            if(inMidWhite (x0, y0)) then -- инициализация в центре белых клеток
                (Cell (x0, y0) (Player WhiteMove)) : (createLine (n-1) (x+cellWidth) y)
            else (Cell (x0, y0) Empty) : (createLine (n-1) (x+cellWidth) y)
    | otherwise = []
    where (x0, y0) = pointToPos (x, y)
-- | преобразование внутреннего представления во внешнее
worldToPicture :: World -> Picture
worldToPicture (World _ _ _ _ _ Menu _ _ _) =   -- ^ режим меню
        pictures[color (makeColor 1 0 0 1) $ polygon [(-200, -200), (200, -200), (200, 200), (-200, 200)]
                , menuText (-60, 160) "MENU"
                , color (makeColor 1 0 1 1) $ polygon [(-105, 110), (110, 110), (110, 140), (-105, 140)]
                , menuText (-105, 110) "Start game"
                , color (makeColor 1 0 1 1) $ polygon [(-160, 60), (170, 60), (170, 90), (-160, 90)]
                , menuText (-150, 60) "Return to game"
                , color (makeColor 1 0 1 1) $ polygon [(-100, 10), (115, 10), (115, 40), (-100, 40)]
                , menuText (-100, 10) "Load game"
                , color (makeColor 1 0 1 1) $ polygon [(-190, -40), (190, -40), (190, -10), (-190, -10)]
                , menuText (-190, -40) "View loaded game"]
worldToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view) 
    | cntBlack + cntWhite == 64 && (cntBlack == cntWhite) = 
        Pictures(
        (insertText (posToPoint (-3, 7)) "DRAW") : (textSave svg) : (insertText (posToPoint (-7, -7)) "Space - back to Menu") :
        boardToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view))
    | (cntBlack + cntWhite == 64) && (cntBlack < cntWhite) = 
        Pictures(
        (insertText (posToPoint (0, 10)) "White wins") : (textSave svg) :(insertText (posToPoint (-7, -7)) "Space - back to Menu") :
        boardToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view))
    | cntBlack + cntWhite == 64 && (cntBlack > cntWhite) = 
        Pictures(
        (insertText (posToPoint (0, 10)) "Black wins") : (textSave svg) :(insertText (posToPoint (-7, -7)) "Space - back to Menu") :
        boardToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view))
    | otherwise = case state of
        Player WhiteMove ->
            Pictures(
            (insertText (posToPoint (-3, 7)) "White") : (insertText (posToPoint (-7, -7)) "Space - back to Menu") :
            (insertTextNumb (posToPoint (-5,5)) "White" cntWhite) :
            (insertTextNumb (posToPoint (5,5)) "Black" cntBlack) : (textSave svg) :
            boardToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view))
        _ ->
            Pictures(
            (insertText (posToPoint (-3, 7)) "Black") : (insertText (posToPoint (-7, -7)) "Space - back to Menu") :
            (insertTextNumb (posToPoint (-5, 5)) "White" cntWhite) :
            (insertTextNumb (posToPoint (5, 5)) "Black" cntBlack) : (textSave svg) :
            boardToPicture (World world state (cntBlack, cntWhite) prevW m stg svg stplst view))

--возвращает список картинок, в зависимости от State 
boardToPicture :: World -> [Picture]
boardToPicture (World ((Cell xy Empty) : xs) k cnt prevW m stg svg stplst view)              = (addEmptyChecker  (posToPoint xy) m) ++ (boardToPicture (World xs k cnt prevW m stg svg stplst view))
boardToPicture (World ((Cell xy (Player BlackMove)) : xs) k cnt prevW m stg svg stplst view) = (addBlackChecker  (posToPoint xy) m) ++ (boardToPicture (World xs k cnt prevW m stg svg stplst view))
boardToPicture (World ((Cell xy (Player WhiteMove)) : xs) k cnt prevW m stg svg stplst view) = (addWhiteChecker (posToPoint xy) m) ++ (boardToPicture (World xs k cnt prevW m stg svg stplst view))
boardToPicture (World ((Cell xy PossibleMove) : xs) k cnt prevW m stg svg stplst view)       = (addPosChecker  (posToPoint xy) m) ++ (boardToPicture (World xs k cnt prevW m stg svg stplst view))
boardToPicture _ = []
-- | нарисовать текст "Игра сохранена"
textSave :: (Maybe World, Int) -> Picture
textSave (Just _, 1) = insertText (posToPoint (-10, 10)) "Game saved"
textSave _ = polygon[(0, 0)]
--"реагирование" на нажатую кнопку
handleEvents :: Event -> World -> World
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) w 
    | gamestate w == View = (fst(stepsList w) !! 0){gamestate = Menu} -- | с помощью пробела можно перейти в режим меню
    | gamestate w == Game = w{gamestate = Menu, stepsList = (w : (fst $ stepsList w), snd $ stepsList w)}
    | otherwise = w
handleEvents (EventKey (Char 's') Down _ _) w                                  -- | нажав на "s"
    | gamestate w == Game = w{savedGame = (Just w{stepsList = stplst}, 1), stepsList = stplst} -- | в режиме игры мы можем сохранить игру 
    | otherwise = w                                                                            -- | но не в режиме меню
    where
        len = length(fst $ stepsList w) - 1
        stplstlst = if (snd $ stepsList w) == 0 || prevWorld ((fst $ stepsList w) !! (len - (snd $ stepsList w))) /= Nothing then (fst $ stepsList w) 
                    else deleteTail (fst $ stepsList w) (len - (snd $ stepsList w)) 0
        stplst = (stplstlst, length(stplstlst) + 1)
handleEvents (EventKey (MouseButton LeftButton) Down _ (x, y)) w -- | если нажата левая кнопка мыши 
    | gamestate w == Game = move (pointToPos (x,y))  w           -- | если мы в режиме игры, проверяем какая часть поля нажта
    | gamestate w == Menu && fst(savedGame w) /= Nothing &&                                                 -- | если игра сохранена
        x >= -100 && x <= 100 && y >= 10 && y <= 40 = (unJust $ fst (savedGame w)){savedGame = savedGame w } -- | мы можем её загрузить, нажав на "Load game"
    | gamestate w == Menu && fst(savedGame w) /= Nothing &&                                                 -- | если игра сохранена
        x >= -190 && x <= 190 && y >= -40 && y <= -10 = 
        viewGame (fst(stepsList w) !! (length(fst $ stepsList w) - 1)){gamestate = View, savedGame = savedGame w, stepsList = stepsList w, viewed = length(fst $ stepsList w)} 
    | gamestate w == Menu = analyseMenuClic x y w                          -- | если мы в режиме меню, проверяем какая кнопка нажата
    | otherwise = w
handleEvents (EventKey (MouseButton RightButton) Down _ _) w = goToBack w   -- | переходим на пред шаг правой кнопкой мыши
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) w 
    | gamestate w == View = viewGame w 
    | otherwise = w
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) w
    | gamestate w == View = viewBackGame w 
    | otherwise = w
handleEvents (EventMotion (x, y)) w
    | gamestate w == Menu || gamestate w == Game = w{mouse = (x, y)}  -- | движение мышки
    | otherwise = w
handleEvents _ w | gamestate w == Game = drawPosToMove w -- | если ничего не нажато, рисуем возможные позиции для хода в режиме игры
                 | otherwise = w                         -- | в режиме меню ничего не делаем

-- | откат назад
goToBack :: World -> World
goToBack w | gamestate w == Menu || prevWorld w == Nothing || gamestate w == View = w
           | otherwise = (unJust(prevWorld w)){savedGame = (fst(savedGame w), 0)}
-- | просмотр сохранённой игры
viewGame :: World -> World
viewGame w  | (viewed w) == (length(fst $ stepsList w) - (snd $ stepsList w)) = w
            | otherwise = (fst(stepsList w) !! ((viewed w) - 1)){gamestate = View, savedGame = savedGame w, stepsList = stepsList w, viewed = ((viewed w) - 1)}

viewBackGame :: World -> World
viewBackGame w | (viewed w)  == (length(fst $ stepsList w) - 1) = w
               | otherwise = (fst(stepsList w) !! ((viewed w) + 1)){gamestate = View, savedGame = savedGame w, stepsList = stepsList w, viewed = ((viewed w) + 1)}
-- | реагирование на нажатую левую кнопку мышки
move :: Pos -> World -> World
move p w
    | checkSelectedPos p (worldCells w) =                                       -- | игрок выбрал возможный ход?
        changeTurn $ del $ reColorLine p $ markChoosedCell p w{savedGame = svg, stepsList = stplst}
    | noMove (worldCells w) = changeTurn w{savedGame = svg, stepsList = stplst} -- | если не осталось возможных ходов, передаем ход другому
    | otherwise = w             -- | игрок выбрал позицию, куда нельзя ставить
    where
        svg = (fst(savedGame w), 0)
        stplst = (w : fst(stepsList w), snd(stepsList w))
-- проверяет, остались ли возможные ходы, если нет, то передаем ход
noMove :: [Cell] -> Bool 
noMove ((Cell _ PossibleMove) : _) = False
noMove (_:xs) = (noMove xs)
noMove [] = True
-- передаем ход игрока
changeTurn :: World -> World
changeTurn (World cell (Player BlackMove) total prevW m stg svg stplst view) = 
    (World cell (Player WhiteMove) total prevW m stg svg stplst view)
changeTurn (World cell (Player WhiteMove) total prevW m stg svg stplst view) = 
    (World cell (Player BlackMove) total prevW m stg svg stplst view)
changeTurn _ = undefined -- | that's impossible
-- выбрал ли игрок, позицию куда можно ходить?
checkSelectedPos :: Pos -> [Cell] -> Bool 
checkSelectedPos p ((Cell p1 state) : xs) 
    | (areal p p1)&&(isStatePossibleMove state) = True
    | otherwise = (checkSelectedPos p xs)
checkSelectedPos _ [] = False
-- помечаем, выбранную игроком, позицию
-- сохраняем пред World
markChoosedCell :: Pos -> World -> World 
markChoosedCell p (World cell turn total prevW m stg svg stplst view) = 
    (World (markCell p cell turn) turn total (Just (World cell turn total prevW m Game svg stplst view)) m stg svg stplst view)
-- помечаем, выбранную игроком, позицию
markCell :: Pos -> [Cell] -> State ->[Cell]
markCell p ((Cell p1 state) : xs) turn
    | ((areal p p1) && (isStatePossibleMove state)) = ((Cell p1 turn) : xs)
    | otherwise = (Cell p1 state) : (markCell p xs turn)
markCell _ [] _ = undefined -- | не должно, чтоьы он не нашел
-- замена позиции, помеченные "крестиком", на пустые
del :: World -> World
del w = w{worldCells = delX (worldCells w)}
-- заменяем "возможные ходы" на пустые позиции
delX :: [Cell] -> [Cell]
delX ((Cell p state) : xs) 
    | (isStatePossibleMove state) = (Cell p Empty) : (delX xs)
    | otherwise = (Cell p state) : (delX xs)
delX [] = []
-- | нажатие на кнопки меню
analyseMenuClic :: Float -> Float -> World -> World
analyseMenuClic x y w
    | x >= -105 && x <= 110 && y >= 110 && y <= 140 -- | тут расположена кнопка "Start game"
                = (World (createBoard 7 initialLocation) (Player WhiteMove) (2,2) Nothing (-120, -120) Game (savedGame w) stplst 0) -- | новая игра с сохранением
    | x >= -160 && x <= 170 && y >= 60 && y <= 90  -- | "Return to game"
                = w{gamestate = Game} -- | меняем режим Меню на режим Игра
    | otherwise = w -- | не нажата никакая кнопка
    where
         stplst = if (fst $ savedGame w) == Nothing then ([], 0)
                  else (deleteHead (fst $ stepsList w) (snd $ stepsList w), (snd $ stepsList w))

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
insertBackPic :: (Float,Float) -> Picture
insertBackPic (x, y) = 
    Translate x y
    $ Scale 0.3 0.3
    $ Color red
    $ unsafePerformIO (loadBMP "data/back.bmp")
-- вставка текста
insertText :: (Float,Float) -> String -> Picture
insertText (x, y) w 
    | w == "Black" = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color black
        $ Text $ w ++ "'s moving" -- | Black's moving
    | w == "White" = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color white
        $ Text $ w ++ "'s moving" -- | White's moving
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
    | w == "DRAW" =
        Translate x y
        $ Scale 0.4 0.4
        $ Color black
        $ Text $ w
    | otherwise = 
        Translate x y
        $ Scale 0.4 0.4
        $ Color white
        $ Text $ w 
-- | Рисуем текст на меню        
menuText :: (Float,Float) -> String -> Picture
menuText (x, y) w = Translate x y $ Scale 0.3 0.3 $ Color yellow $ Text w
-- инициализируем черные клетки в центр
inMidBlack :: (Int, Int) -> Bool
inMidBlack (x, y) = x == y && (x == 3+k || x == 4+k)
    where k = (fst (pointToPos initialLocation))
-- инициализируем белые клетки в центр
inMidWhite :: (Int, Int) -> Bool
inMidWhite (x, y) = (x+y==7+2*k) && (x == 3+k || x == 4+k)
    where k = (fst (pointToPos initialLocation))
-- return Black Picture
addBlackChecker :: (Float, Float) -> (Float, Float) -> [Picture]
addBlackChecker (x, y) m = [Translate (x + 0.5 * cellWidth) (y + 0.5 * cellHeight)
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/black.bmp") ] ++ [addMousePolygon (pointToPos (x, y)) (pointToPos m)]
-- return Possible Move Picture
addPosChecker :: (Float, Float) -> (Float, Float) -> [Picture]
addPosChecker (x, y) m = [Translate (x + 0.5 * cellWidth) (y + 0.5 * cellHeight)
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- |почему 0,009? нужно для того, чтобы квадратики не слипались 
    $ unsafePerformIO(loadBMP "data/canMove.bmp")] ++ [addMousePolygon (pointToPos (x, y)) (pointToPos m)]
-- return White Picture
addWhiteChecker :: (Float, Float) -> (Float, Float) -> [Picture]
addWhiteChecker (x, y) m = [Translate (x + 0.5 * cellWidth) (y + 0.5 * cellHeight)
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/white.bmp")] ++ [addMousePolygon (pointToPos (x, y)) (pointToPos m)]
-- Return Green Picture
addEmptyChecker :: (Float, Float) -> (Float, Float) -> [Picture]
addEmptyChecker (x, y) m = [Translate (x + 0.5 * cellWidth) (y + 0.5 * cellHeight)
    $ Scale (0.009*cellWidth) (0.009*cellHeight) -- | привязка к ширине и высоте нашего квадратика
    $ unsafePerformIO(loadBMP "data/green.bmp")] ++ [addMousePolygon (pointToPos (x, y)) (pointToPos m)]
-- Return red polygon
addMousePolygon:: Pos -> Pos -> Picture
addMousePolygon (x1, y1) (x2, y2) | (areal (x1, y1) (x2, y2)) = color (makeColor 1 0 0 0.5) $
    polygon [(a , b), (a + cellWidth, b), (a + cellWidth, b + cellHeight), (a, b + cellHeight)]
                                  | otherwise = color (makeColor 0 0 0 0) $ polygon [(0, 0)]
    where a = cellWidth * fromIntegral (x1) :: Float
          b = cellHeight * fromIntegral (y1) :: Float