import System.IO (readFile)

type Command = (String, Int)

parseLine :: String -> Command
parseLine line = let [cmd, num] = words line in (cmd, read num)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let commands = map parseLine (lines content)

  print (solvePart1 commands)
  print (solvePart2 commands)

multiplyTuple :: (Int, Int) -> Int
multiplyTuple (h, d) = h * d

solvePart1 :: [Command] -> Int
solvePart1 = multiplyTuple . foldl applyCommand (0, 0)
 where
  applyCommand (h, d) ("forward", x) = (h + x, d)
  applyCommand (h, d) ("down", x) = (h, d + x)
  applyCommand (h, d) ("up", x) = (h, d - x)

solvePart2 :: [Command] -> Int
solvePart2 = multiplyTuple . extractPosition . foldl applyCommandWithAim (0, 0, 0)
 where
  applyCommandWithAim :: (Int, Int, Int) -> Command -> (Int, Int, Int)
  applyCommandWithAim (h, d, aim) ("forward", x) = (h + x, d + aim * x, aim)
  applyCommandWithAim (h, d, aim) ("down", x) = (h, d, aim + x)
  applyCommandWithAim (h, d, aim) ("up", x) = (h, d, aim - x)
  extractPosition (h, d, _) = (h, d)
