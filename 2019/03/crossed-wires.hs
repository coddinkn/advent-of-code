import Prelude hiding (Left, Right)
import Data.Set (Set, insert, intersection, toList, fromList, singleton, union)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)

type Position = (Int, Int)

data Direction = Right
               | Left
               | Up
               | Down
               deriving Show

data Instruction = Instruction Direction Int
                   deriving Show

type Wire = [Instruction]

instance Read Instruction where
    readsPrec _ (dir:rest) = let direction = case dir of
                                                  'R' -> Just Right
                                                  'L' -> Just Left 
                                                  'U' -> Just Up
                                                  'D' -> Just Down
                                                  _   -> Nothing
                                 distance = read rest
                             in maybe [] 
                                      (\d -> [(Instruction d distance, "")]) 
                                      direction

getLastPosition :: Position -> Instruction -> Position
getLastPosition (x, y) (Instruction dir d) =
    case dir of
         Left  -> (x - d, y)
         Right -> (x + d, y)
         Up    -> (x, y - d)
         Down  -> (x, y + d)

getPositions :: Position -> Instruction -> Set Position
getPositions (x, y) (Instruction dir distance) =
    let distances = [1 .. distance]
        change = case dir of
                      Left  -> \d -> (x - d, y)
                      Right -> \d -> (x + d, y)
                      Up    -> \d -> (x, y - d)
                      Down  -> \d -> (x, y + d)
    in fromList $ change <$> distances

getNextPositions :: (Position, Set Position) -> Instruction -> (Position, Set Position)
getNextPositions (position, positions) instruction =
    let nextPosition = getLastPosition position instruction
        nextPositions = union positions $ getPositions position instruction
    in (nextPosition, nextPositions)

getAllPositions :: Wire -> Set Position
getAllPositions = snd . foldl getNextPositions ((0, 0), singleton (0, 0))

getOrderedPositions :: Position -> Instruction -> [Position]
getOrderedPositions (x, y) (Instruction dir distance) =
    let distances = [1 .. distance]
        change = case dir of
                      Left  -> \d -> (x - d, y)
                      Right -> \d -> (x + d, y)
                      Up    -> \d -> (x, y - d)
                      Down  -> \d -> (x, y + d)
    in change <$> distances

getAllOrderedPositions :: Wire -> [Position]
getAllOrderedPositions = foldl getNextOrderedPositions [(0, 0)]

getNextOrderedPositions :: [Position] -> Instruction -> [Position]
getNextOrderedPositions positions instruction = let lastPosition = last positions
                                                    newPositions = getOrderedPositions lastPosition instruction
                                                in positions ++ newPositions

manhattanDistance :: Position -> Int
manhattanDistance (x, y) = abs x + abs y

getIntersections :: Wire -> Wire -> [Position]
getIntersections wire1 wire2 = toList $ intersection (getAllPositions wire1) (getAllPositions wire2)

findClosestCrossing :: Wire -> Wire -> Int
findClosestCrossing wire1 wire2 = let intersections = getIntersections wire1 wire2
                                  in minimum . filter (/= 0) . map manhattanDistance $ intersections

countSteps :: [Position] -> [Position] -> Position -> Maybe Int
countSteps positions1 positions2 position = do steps1 <- elemIndex position positions1
                                               steps2 <- elemIndex position positions2
                                               return $ steps1 + steps2

findShortestCrossing :: Wire -> Wire -> Int
findShortestCrossing wire1 wire2 = let intersections = filter (/= (0, 0)) $ getIntersections wire1 wire2
                                       positions1 = getAllOrderedPositions wire1
                                       positions2 = getAllOrderedPositions wire2
                                   in minimum . catMaybes $ countSteps positions1 positions2 <$> intersections 

getInput :: String -> (Wire, Wire)
getInput input = let [str1, str2] = lines input
                 in (getInstructions str1, getInstructions str2)    
    where getInstructions = map read . words . map (\c -> if c == ',' then ' ' else c) 

main :: IO ()
main = do (wire1, wire2) <- getInput <$> readFile "input.txt"
          putStr "Part 1: "
          print $ findClosestCrossing wire1 wire2
          putStr "Part 2: "
          print $ findShortestCrossing wire1 wire2
