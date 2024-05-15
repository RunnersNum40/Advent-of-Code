import System.IO (readFile)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let depths = map read (lines content) :: [Int]

  let part1Result = countIncreases depths
  print part1Result

  let part2Result = countSlidingWindowIncreases depths
  print part2Result

countIncreases :: (Ord a) => [a] -> Int
countIncreases (x : y : xs) = (if y > x then 1 else 0) + countIncreases (y : xs)
countIncreases _ = 0

countSlidingWindowIncreases :: (Num a, Ord a) => [a] -> Int
countSlidingWindowIncreases depths = countIncreases (slidingWindowSums 3 depths)

slidingWindowSums :: (Num a) => Int -> [a] -> [a]
slidingWindowSums n xs
  | length xs < n = []
  | otherwise = sum (take n xs) : slidingWindowSums n (tail xs)
