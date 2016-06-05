module Types where
-- | 
data World = World
  { worldCells  :: [Cell]           -- ^ клетки
  , worldPlayer :: State            -- ^  чей ход
  , worldTotals :: CountBlackWhite  -- ^ кол-во черныз, белых
  , prevWorld   :: Maybe World      -- ^ храним предыдущий шаг
  , mouse       :: Point            -- ^ считывание позиции мыши
  , gamestate   :: MenuorGame       -- ^ меню или игра
  , savedGame   :: (Maybe World, Int) -- ^ сохранённая игра, 0 или 1
  , stepsList   :: ([World], Int)    -- ^ последовательность действий
  } deriving Eq
-- | собственно "клетка"
data Cell = Cell
  { cellPos   :: Pos      -- ^ x y координата
  , cellState :: State    -- ^ состояние (пустое, занято(белым\черным), помечено "крестиком")
  } deriving Eq
-- | Состояние клетки игрового поля.
data State
  = Empty            -- ^ Пустая клетка.
  | Player WhichMove -- ^ Клетка игрока.
  | PossibleMove     -- ^ Возможный ход.
  deriving Eq
data MenuorGame = Menu | Game | View deriving Eq    -- ^ меню/игра

-- ширина и высота клеток
cellWidth, cellHeight:: Float
cellWidth  = 35 -- | ширина
cellHeight = 35 -- | высота
-- смещение месторасположения
initialLocation :: (Float, Float)
initialLocation = (-100, -100)
-- | ход черных \ белых
data WhichMove = BlackMove | WhiteMove  deriving Eq 
-- | кол-во черных \ белых
type CountBlackWhite = (CntBlack, CntWhite)
-- | x y координата
type Pos = (Int, Int)
-- | кол-во черных клеток
type CntBlack = Int
-- | кол-во белых клеток
type CntWhite = Int
-- | координата курсора мыши
type Point = (Float, Float)
-- | вещ х y координату -> целочисл x y
pointToPos :: (Float, Float) -> (Int, Int)
pointToPos (x, y) = (floor(x / cellWidth), floor(y / cellHeight))
-- | целочисл x y -> вещ х y координату  
posToPoint :: (Int, Int) -> (Float, Float)
posToPoint (x, y) = ((a) * cellWidth, (b) * cellHeight)
    where a = fromIntegral (x) :: Float
          b = fromIntegral (y) :: Float
-- сравнивает координаты
areal :: Pos -> Pos -> Bool
areal (x1, y1) (x, y) = x1 == x && y1 == y
-- сравнивает состояния игроков
isEqMove :: WhichMove -> WhichMove -> Bool
isEqMove BlackMove BlackMove = True
isEqMove WhiteMove WhiteMove = True
isEqMove _ _ = False
-- State is equal to PossibleMove ?
isStatePossibleMove :: State -> Bool
isStatePossibleMove PossibleMove = True
isStatePossibleMove _ = False

unJust :: Maybe a -> a
unJust (Just a) = a
unJust _ = undefined
   

