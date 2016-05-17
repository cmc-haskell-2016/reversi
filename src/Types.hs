module Types where

-- | asdkfhlsdkfhj
data World = World
  { worldCells  :: [Cell]           -- ^ дфывлаофывда
  , worldPlayer :: Player           -- ^  s;lkfgjs;lfdkgj
  , worldTotals :: CountBlackWhite  -- ^ ыдаплоыадпло
  }

type Pos = (Float, Float)

-- | ылопрыфдлвоар
data Cell = Cell
  { cellPos   :: Pos      -- ^ sldjfhasdf
  , cellState :: State    -- ^ skljhfalkjdf
  }

-- | Состояние клетки игрового поля.
data State
  = Empty           -- ^ Пустая клетка.
  | Player Player   -- ^ Клетка игрока.
  | PossibleMove    -- ^ Возможный ход.

-- | sdkfjhasdlkfj
type CountBlackWhite = (Int, Int)

cellWidth, cellHeight :: Float
cellWidth  = 35
cellHeight = 35

-- | ?????
initLocation :: Float
initLocation = -100

-- | ???
eps :: Float
eps = 10.0
