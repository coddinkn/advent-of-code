import Data.List

type House = (Int, Int)

change char = case char of
    'v' -> ( 0, -1)
    '^' -> ( 0,  1)
    '>' -> ( 1,  0)
    '<' -> (-1,  0)

initial :: (House, [House])
initial = ((0, 0), [(0, 0)])

nextHouse :: (House, [House]) -> (Int, Int) -> (House, [House])
nextHouse ((x, y), houses) (dx, dy) = 
    if next `notElem` houses
    then (next, insert next houses)
    else (next, houses)                               
    where next = (x + dx, y + dy)

partOne = length . snd . foldl nextHouse initial



main = do changes <- map change <$> readFile "input.txt"
          print $ partOne changes

