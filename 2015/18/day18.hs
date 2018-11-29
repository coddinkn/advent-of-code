import System.Environment

type Light     = Char
type LightGrid = [[Light]]

isOn :: Light -> Bool
isOn = (==) '#'

getLight :: LightGrid -> Int -> Int -> Light
getLight grid x y
    | x >= 0 && x < w && y >= 0 && y < h = (grid !! x) !! y
    | otherwise                          = '.'
    where w = length grid
          h = length $ head grid

neighbors :: LightGrid -> Int -> Int -> [Light]
neighbors grid x y = [light 0 1, light 1 0, light (-1) 0, light 0 (-1), light 1 1, light (-1) (-1), light 1 (-1), light (-1) 1]
    where light offsetX offsetY = getLight grid (x + offsetX) (y + offsetY)

countOnNeighbors :: LightGrid -> Int -> Int -> Int
countOnNeighbors grid x y = length $ filter isOn $ neighbors grid x y

mapOverMatrix :: Int -> Int -> (Int -> Int -> a) -> [[a]]
mapOverMatrix width height f = map (\n -> map (f n) [0..(height - 1)]) [0..(width - 1)]

getOnNeighborsGrid :: LightGrid -> [[Int]]
getOnNeighborsGrid grid = mapOverMatrix width height count 
    where count  = countOnNeighbors grid
          width  = length grid
          height = length $ head grid

pairWithOnState :: LightGrid -> [[Int]] -> [[(Bool, Int)]]
pairWithOnState lights neighborCount = zipWith zip (map (map isOn) lights) neighborCount

stepFunction :: (Bool, Int) -> Light
stepFunction (on, neighbors)
    | on == True && ((neighbors == 3) || (neighbors == 2)) = '#'
    | on == False && neighbors == 3                        = '#'
    | otherwise                                            = '.'

takeOneStep :: LightGrid -> LightGrid 
takeOneStep grid = map (map stepFunction) statusGrid
    where statusGrid = pairWithOnState grid $ getOnNeighborsGrid grid

isCorner :: Int -> Int -> Int -> Int -> Bool
isCorner w h x y
    | x == (w - 1) && y == 0       = True
    | x == 0 && y == (h - 1)       = True
    | x == (w - 1) && y == (h - 1) = True
    | x == 0 && y == 0             = True
    | otherwise                    = False

turnOnCorners :: LightGrid -> LightGrid
turnOnCorners grid = mapOverMatrix width height corners
    where corners x y = if isCorner width height x y then '#' else getLight grid x y
          width       = length grid
          height      = length $ head grid

takeNSteps :: Int -> LightGrid -> LightGrid
takeNSteps n grid
    | n > 0     = takeNSteps (n - 1) (takeOneStep grid)
    | otherwise = grid

newTakeNSteps :: Int -> LightGrid -> LightGrid
newTakeNSteps n grid
    | n > 0     = newTakeNSteps (n - 1) (takeOneStep $ turnOnCorners grid)
    | otherwise = turnOnCorners grid
 
main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let grid     = lines input
    let after100 = takeNSteps 100 grid
    let corners  = newTakeNSteps 100 grid
    print $ sum $ map (length . filter isOn) $ after100
    print $ sum $ map (length . filter isOn) $ corners
