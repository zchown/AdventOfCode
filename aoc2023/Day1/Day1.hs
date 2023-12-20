import Data.Char (digitToInt, isDigit)
import Data.List (findIndex, isPrefixOf)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print . sum . map part1 $ input
  print . sum . map part2 $ input

firstAndLast :: String -> Int
firstAndLast s = read (head s : [last s])

-- not at all optimized I should just check the first and last
part1 :: String -> Int
part1 = firstAndLast . filter (`elem` ['0' .. '9'])

part2 :: String -> Int
part2 s = firstNum numbers s * 10 + firstNum (map reverse numbers) (reverse s)

numbers :: [String]
numbers =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

firstNum :: [String] -> String -> Int
firstNum nums s@(c:xs) =
  if isDigit c
    then read [c]
    else case findIndex (`isPrefixOf` s) nums of
           Nothing -> firstNum nums xs
           (Just i) -> i + 1
