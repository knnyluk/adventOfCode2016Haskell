{-# LANGUAGE ViewPatterns #-}

import Data.Char
import Data.List
import Data.Monoid
import Control.Arrow

main :: IO ()
main = print . foldr helper 0 . lines =<< readFile "/Users/jasonstolaruk/Dropbox (Personal)/Code/Haskell/input.txt"

helper :: String -> Int -> Int
helper xs acc = let (roomName, rest    ) = break isDigit xs
                    (sectorId, init . tail -> checkSum) = span isDigit rest
                    pairs  = map (head &&& length) . group . dropWhile (== '-') . sort $ roomName
                    sorted = sortBy sorter pairs
                in if map fst (take 5 sorted) == checkSum
                  then acc + read sectorId
                  else acc
  where
    sorter (a, b) (a', b') = b' `compare` b <> a `compare` a'

