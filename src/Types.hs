module Types where


type World = (Pos, State)
type Pos = (Int, Int)--position x,y
type State = Int --0 - empty, 1-black, 2-white

sizeX :: Int
sizeX = 100
sizeY :: Int
sizeY = 125
