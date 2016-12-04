import System.IO
import Data.List

main = do
  input <- readFile "input.txt"
  putStr $ show $ validTrianglesByRow $ lines input
  putStrLn " valid triangles by row"
  putStrLn "sample rows"
  putStrLn $ show $ sum $ map colToTriangles $ linesToColumns $ lines input

colToTriangles :: [Int] -> Int
colToTriangles [] = 0
colToTriangles xs = if areValidSides
                       then colToTriangles remaining + 1
                       else colToTriangles remaining
  where
    (sides,remaining) = splitAt 3 xs
    (a:b:c:_) = sort sides
    areValidSides = 3 == length sides && c < a + b

linesToColumns :: [String] -> [[Int]]
linesToColumns lines = transpose $ linesToIntRows lines

linesToIntRows :: [String] -> [[Int]]
linesToIntRows = foldr (\l acc -> (map strToInt $ words l):acc)  []

validTrianglesByRow :: [String] -> Int
validTrianglesByRow strs = length $ filter isValidTriangle $ parseLinesToTriangles strs

data Triangle = Triangle { a :: Int
                         , b :: Int
                         , c :: Int } deriving (Show)

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
    sortedSides = sort $ map strToInt $ words str
    makeTriangle (a:b:c:excessSides) = if null excessSides
                                          then Triangle a b c
                                          else error "too many sides!"
    makeTriangle _                   = error "you can only make a triangle with exactly 3 sides"

strToInt s = read s::Int
