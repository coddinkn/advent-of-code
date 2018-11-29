import Data.List

toChange char | char == '(' =  1
              | char == ')' = -1
              | otherwise   =  0

partOne = sum . map toChange

partTwo input = (+1) <$> basement `elemIndex` floorAtPosition 
    where floorAtPosition = map (sum . flip take changes) [1..len]
          changes = map toChange input
          len = length input
          basement = (-1)

main = do input <- readFile "input.txt"
          print $ partOne input          
          putStrLn $ case partTwo input of
                          Just position -> show $ position
                          Nothing       -> "didn't happen"
