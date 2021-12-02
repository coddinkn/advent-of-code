import Data.List (foldl')

type Position = (Int, Int) -- (Depth, Horizontal position)

data Direction = Down | Up | Forward

instance Read Direction where
    readsPrec _ "down"    = [(Down, "")]
    readsPrec _ "up"      = [(Up, "")]
    readsPrec _ "forward" = [(Forward, "")]


data PlannedMovement = PlannedMovement Direction Int -- Distance

instance Read PlannedMovement where
    readsPrec _ string =
        let directionString = takeWhile ((/=) ' ') string
            distanceString  = drop 1 $ dropWhile ((/=) ' ') string
        in  [(PlannedMovement (read directionString) $ read distanceString, "")]

start :: Position
start = (0, 0)

move :: PlannedMovement -> Position -> Position
move (PlannedMovement direction distance) (depth, horizontalPosition) =
    case direction of
        Down    -> (depth + distance, horizontalPosition)
        Up      -> (depth - distance, horizontalPosition)
        Forward -> (depth,     horizontalPosition + distance)

evaluateCourse :: [PlannedMovement] -> Position
evaluateCourse = foldl' (flip move) start

type PositionWithAim = (Position, Int)

startWithAim :: PositionWithAim
startWithAim = (start, 0)

moveWithAim :: PlannedMovement -> PositionWithAim -> PositionWithAim
moveWithAim (PlannedMovement direction delta) ((depth, horizontalPosition), aim) =
    case direction of
        Down    -> ((depth, horizontalPosition), aim + delta)
        Up      -> ((depth, horizontalPosition), aim - delta)
        Forward -> ((depth + (delta * aim), horizontalPosition + delta), aim)

evaluateCourseWithAim :: [PlannedMovement] -> PositionWithAim
evaluateCourseWithAim = foldl' (flip moveWithAim) startWithAim

getResult :: Position -> Int
getResult (d, h) = d * h

main :: IO ()
main = do
    course <- map read . lines <$> readFile "input.txt"
    putStr "Part 1: "
    print . getResult $ evaluateCourse course
    putStr "Part 2: "
    print . getResult . fst $ evaluateCourseWithAim course
