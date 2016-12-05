import System.IO
import Data.List

expectedSideCount = 3

main = do
  input <- readFile "input.txt"
  let sanitizedRows = linesToIntRows expectedSideCount $ lines input

  putStr $ show $ count areValidSides sanitizedRows
  putStrLn " valid triangles by row"
  putStr $ show . sum $ map countValidTrianglesSafe $ transpose sanitizedRows
  putStrLn " valid triangles by col"

countValidTrianglesSafe :: [Int] -> Int
countValidTrianglesSafe sides = countValidTriangles $ truncateExtra expectedSideCount sides

countValidTriangles :: [Int] -> Int
countValidTriangles [] = 0
countValidTriangles xs = if areValidSides sides
                            then countValidTriangles remaining + 1
                            else countValidTriangles remaining
  where
    (sides,remaining) = splitAt expectedSideCount xs

areValidSides :: [Int] -> Bool
areValidSides xs = if expectedSideCount == length xs
                      then let (a:b:c:_) = (sort xs) in c < a + b
                      else error "Wrong number of sides"

linesToIntRows :: Int -> [String] -> [[Int]]
linesToIntRows rowLength = foldr f []
  where
    f xs acc = if rowLength == length potentialNums
                  then (map strToInt potentialNums):acc
                  else acc
      where
        potentialNums = words xs
        strToInt = (\x -> read x::Int)

count :: (a -> Bool) -> [a] -> Int
count f = foldr g 0
  where
    g x acc = if f x then acc + 1 else acc

truncateExtra :: Int -> [a] -> [a]
truncateExtra multLen xs = take numToTake xs
  where
    numToTake = multLen * (length xs) `div` multLen
