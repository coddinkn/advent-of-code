import System.Environment
import Data.List
import Data.List.Split
import Control.Monad

type Distance = Int
type Location = String

type Path = (Location, Location, Distance)

getLocations :: String -> (Location, Location)
getLocations s = (min a b, max a b)
    where a = head atob
          b = last atob
          atob = splitOn " to " $ head $ splitOn " = " s 

getDistance :: String -> Distance
getDistance = read . last . splitOn " = "  

makePath :: String -> Path
makePath s = (fst $ l, snd $ l, d)
    where l = getLocations s
          d = getDistance s

pathToLocations :: Path -> [Location]
pathToLocations (a, b, _) = (a:b:[])

pathToDistance :: Path -> Distance
pathToDistance (_, _, d) = d

makeLocationList :: [Path] -> [Location]
makeLocationList = map head . group . sort . concat . map pathToLocations

getDistanceBetween :: [Path] -> Location -> Location -> Distance
getDistanceBetween paths a b = pathToDistance $ head $ filter (\(c, d, _) -> c == a && b == d) paths 

getTotalPathDistance :: [Path] -> [Location] -> Distance
getTotalPathDistance paths (a:b:rest) = if min a b == a
    then (getDistanceBetween paths a b) + (getTotalPathDistance paths (b:rest))
    else (getDistanceBetween paths b a) + (getTotalPathDistance paths (b:rest))
getTotalPathDistance _ _ = 0

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let strings = lines input
    let paths = map makePath strings
    let locations = makeLocationList paths
    let possiblePaths = permutations locations
    putStrLn $ show $ maximum $ map (getTotalPathDistance paths) possiblePaths
