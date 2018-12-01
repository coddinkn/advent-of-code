import Data.List

change char | char == '(' =  1
            | char == ')' = -1
            | otherwise   =  0

partOne = sum

partTwo changes = (+1) <$> basement `elemIndex` floorAtPosition 
    where floorAtPosition = map (sum . flip take changes) [1..len]
          len = length changes
          basement = (-1)

main = do changes <- map change <$> readFile "input.txt"
          print $ partOne changes          
          putStrLn $ case partTwo changes of
                          Just position -> show $ position
                          Nothing       -> "didn't happen"
