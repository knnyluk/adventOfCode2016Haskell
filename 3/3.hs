import System.IO
import Data.List

main = do
  input <- readFile "input.txt"
  let sanitizedRows = linesToIntRows 3 $ lines input
  putStr $ show $ countValidTriangles $ concat sanitizedRows
  putStrLn " valid triangles by row"
  putStr $ show $ sum $ map countValidTriangles . (evenMultiples 3) $ transpose sanitizedRows
  putStrLn " valid triangles by col"

countValidTriangles :: [Int] -> Int
countValidTriangles [] = 0
countValidTriangles xs = if areValidSides
                            then countValidTriangles remaining + 1
                            else countValidTriangles remaining
  where
    (sides,remaining) = splitAt 3 xs
    (a:b:c:_) = sort sides
    areValidSides = c < a + b

linesToIntRows :: Int -> [String] -> [[Int]]
linesToIntRows rowLength = foldr f []
  where
    f xs acc = if rowLength == (length $ potentialNums)
                  then (map strToInt $ potentialNums):acc
                  else acc
      where
        potentialNums = words xs
        strToInt = (\x -> read x::Int)

evenMultiples :: Int -> [a] -> [a]
evenMultiples multLen xs = take numToTake xs
  where
    numToTake = multLen * (length xs) `div` multLen
