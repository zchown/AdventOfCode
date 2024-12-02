module Day2 where

readFileToList :: String -> IO [[Int]]
readFileToList filename = do
  contents <- readFile filename
  return $ map (map read . words) $ lines contents

checkSafety :: [Int] -> Bool
checkSafety [] = True
checkSafety [_] = True
checkSafety z@(x:y:xs)
  | x > y = cs (>) z
  | x < y = cs (<) z
  | otherwise = False
  where
    cs :: (Int -> Int -> Bool) -> [Int] -> Bool
    cs _ [] = True
    cs _ [_] = True
    cs f (x:y:xs)
      | f x y && abs (x - y) <= 3 = cs f (y : xs)
      | otherwise = False

runProblem1 :: IO ()
runProblem1 = do
  x <- readFileToList "inputs/input2.txt"
  print $ length $ filter checkSafety x

checkSafetyDampened :: [Int] -> Bool
checkSafetyDampened xs =
  foldr
    (\x acc -> checkSafety (take x xs ++ drop (x + 1) xs) || acc)
    False
    [0 .. length xs - 1]

runProblem2 :: IO ()
runProblem2 = do
  x <- readFileToList "inputs/input2.txt"
  print $ length $ filter checkSafetyDampened x
