module Types where


type World = (Pos, State)
type Pos = (Float, Float)--position x,y
type State = Int --0 - empty, 1-black, 2-white

offsetX :: Float
offsetX = 35.0
offsetY :: Float
offsetY = 35.0
initLocation :: Float
initLocation = (-100.0)