import Data.List.Unique

parseChanges :: String -> Int
parseChanges line = case line of
    '+':rest  -> read rest
    otherwise -> read line

partOne = sum

frequencies :: [Int] -> [Int] -> [Int]
frequencies past (change:changes)
    | current `elem` past = newPast
    | otherwise = frequencies newPast changes
    where current = head past + change
          newPast = current:past
frequencies past [] = past

initial = [0]

partTwo = head . frequencies initial . cycle

main = do changes <- map parseChanges . lines <$> readFile "input.txt"
          print $ partOne changes
          print $ partTwo changes
