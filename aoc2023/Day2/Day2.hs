import Data.List (isInfixOf)
import Data.List.Split

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  gs <- (checkGames 14 13 12) . map createGame $ input
  print gs

data Game = Game
  { id :: Int
  , maxBlue :: Int
  , maxGreen :: Int
  , maxRed :: Int
  }

checkGames :: Int -> Int -> Int -> [Game] -> [Game]
checkGames b g r = filter checkGame
  where
    checkGame gm
      | maxBlue gm > b = False
      | maxGreen gm > g = False
      | maxRed gm > r = False
      | otherwise = True

createGame :: String -> Game
createGame s = Game i 0 0 0
  where
    s' = splitOneOf ";,:" s
    i :: Int = (read . last . words . head) s'
    b = findMax "blue" s'
    g = findMax "green" s'
    r = findMax "red"

findMax :: String -> [String] -> Int
findMax s = maximum . map (read . head . words) . filter (isInfixOf s)
