import Data.Function (fix)
import System.IO

-- memoize :: (Int -> a) -> (Int -> a) -> (Integer -> a)
-- memoize f = (map f [0 ..] !!)

-- fishes :: Int -> Int -> Integer
-- fishes n days
--     |   n >= days || days == 0 = 1
--     |   n == days+1 = 2
--     |   otherwise fishes(7, days-n)+fishes(9, days-n)

-- fishesMemo :: Int -> Int -> Integer
-- fishesMemo = fix (memoize . fishes)

main :: IO ()
main = do
    contents <- readFile "test.txt"
    print . map readInt . (split "," contents)

split :: Char -> String -> [String]
split _ "" = []
split delimiter str = 
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

readInt :: String -> [Int]
readInt = read