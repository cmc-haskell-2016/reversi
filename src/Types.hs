module Types where

type World = ([WorldObject], WhichMove)
type WorldObject = (Pos, State)
type WhichMove = Int -- 1 - ход черных, 2- ход белых	
type Pos = (Float, Float)--position x,y
type State = Int --0 - empty, 1-black, 2-white

offsetX :: Float
offsetX = 35.0
offsetY :: Float
offsetY = 35.0
initLocation :: Float
initLocation = (-100.0)
eps :: Float
eps = 10.0
