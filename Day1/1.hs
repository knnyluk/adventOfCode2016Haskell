import Data.Char
import Data.List.Split

main = do
  input <- readFile "input.txt"
  let instructions = splitOn ", " input

  let finalPos = foldl newPosition (Position (Vector 0 0) North) instructions

  putStr . show $ calcDisplacement finalPos

data Bearing = North | East | South | West deriving (Eq, Show, Enum, Bounded)

turn :: Bearing -> Char -> Bearing
turn North 'L' = maxBound
turn dir 'L' = pred dir
turn West 'R' = minBound
turn dir 'R' = succ dir

data Vector = Vector Int Int deriving (Show, Eq)

eigenVector :: Bearing -> Vector
eigenVector dir = case dir of
                    North -> Vector 0 1
                    East -> Vector 1 0
                    South -> Vector 0 (negate 1)
                    West -> Vector (negate 1) 0

vplus :: Vector -> Vector -> Vector
(Vector x y) `vplus` (Vector x' y') = Vector (x + x') (y + y')

vMult :: Vector -> Int -> Vector
(Vector x y) `vMult` m = Vector (x*m) (y*m)

data Position = Position Vector Bearing deriving (Show)

type Move = String

newPosition :: Position -> Move -> Position
newPosition (Position v1 b1) (turnDir:steps) = Position (v1 `vplus` v2) newBearing
  where
    newBearing = turn b1 turnDir
    v2 = (eigenVector newBearing) `vMult` read steps

calcDisplacement :: Position -> Int
calcDisplacement (Position (Vector x y) _) = (abs x) + (abs y)
