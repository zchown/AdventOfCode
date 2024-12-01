module Day1 where

import Data.Composition ((.:))
import Data.Function (on)
import Data.List (sort)

readFileToList :: String -> IO [[Int]]
readFileToList filename = do
  contents <- readFile filename
  let temp = map (map read . words) $ lines contents
  let p1 = map head temp
  let p2 = map last temp
  return [p1, p2]

distances :: [Int] -> [Int] -> Int
distances = sum .: map abs .: zipWith (-)

runProblem1 :: IO ()
runProblem1 = do
  (x:y:_) <- readFileToList "inputs/input1.txt"
  print $ distances (sort x) (sort y)

numberOfMatches :: Int -> [Int] -> Int
numberOfMatches = length .: filter . (==)

similarity :: [Int] -> [Int] -> Int
similarity xs = foldr (\x acc -> acc + x * numberOfMatches x xs) 0

runProblem2 :: IO ()
runProblem2 = do
  (x:y:_) <- readFileToList "inputs/input1.txt"
  print $ similarity x y
