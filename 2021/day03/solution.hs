import Data.Char (digitToInt, isDigit)
import Data.List (maximumBy, minimumBy, transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import System.IO (readFile)

parseLine :: String -> [Int]
parseLine = map digitToInt . filter isDigit

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsedLines = map parseLine (lines content)

  print (solvePart1 parsedLines)
  print (solvePart2 parsedLines)

binaryToInt :: [Int] -> Int
binaryToInt = foldl (\acc x -> acc * 2 + x) 0

countOccurrences :: (Ord a) => [a] -> [(a, Int)]
countOccurrences = Map.toList . Map.fromListWith (+) . map pair
 where
  pair x = (x, 1)

mostCommon :: (Ord a) => [a] -> a
mostCommon = fst . maximumBy (comparing snd) . countOccurrences

leastCommon :: (Ord a) => [a] -> a
leastCommon = fst . minimumBy (comparing snd) . countOccurrences

solvePart1 :: [[Int]] -> Int
solvePart1 report = binaryToInt (gamma report) * binaryToInt (epsilon report)
 where
  gamma = map mostCommon . transpose
  epsilon = map leastCommon . transpose

sieve :: (Eq a) => Int -> ([a] -> a) -> [[a]] -> [a]
sieve _ _ [x] = x
sieve n criteria xs = sieve (n + 1) criteria filtered
 where
  match = criteria (map (!! n) xs)
  filtered = filter (\x -> x !! n == match) xs

solvePart2 :: [[Int]] -> Int
solvePart2 report = binaryToInt (oxygen report) * binaryToInt (co2 report)
 where
  oxygen = sieve 0 mostCommon
  co2 = sieve 0 leastCommon
