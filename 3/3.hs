import System.IO
import Data.List

main = do
  input <- readFile "input.txt"
  putStr $ show $ length $ filter isValidTriangle $ parseLinesToTriangles $ lines input

data Triangle = Triangle { a :: Int
                         , b :: Int
                         , c :: Int} deriving (Show)

isValidTriangle :: Triangle -> Bool
isValidTriangle t = c t < a t + b t

parseLinesToTriangles :: [String] -> [Triangle]
parseLinesToTriangles = foldr f []
  where
    f line acc = if (length $ words line) == 3
                    then (strToTriangle line):acc
                    else acc

strToTriangle :: String -> Triangle
strToTriangle str = makeTriangle sortedSides
  where
    sortedSides = sort $ map (\s -> read s::Int) $ words str
    makeTriangle (a:b:c:excessSides) = if null excessSides
                                          then Triangle a b c
                                          else error "too many sides!"
    makeTriangle _                   = error "you can only make a triangle with exactly 3 sides"

