import System.Environment
import Data.List
import Data.List.Split
import Control.Monad

type Person    = String
type Happiness = Int

type SeatingArrangement    = [Person]
type NeighborConfiguration = (Person, Person, Happiness) 

getAllPeople :: [NeighborConfiguration] -> [Person]
getAllPeople = map head . group . sort . map (\(a, _, _) -> a)

getConfigurations :: String -> NeighborConfiguration
getConfigurations s = (a, b, h)
    where (a, b) = getPeople s
          h      = getHappiness s

getPeople :: String -> (Person, Person)
getPeople s = (a, b)
    where w = words s
          a = head w
          b = init $ last w

getHappiness :: String -> Happiness
getHappiness s
    | (w !! 2) == "gain" = read (w !! 3)
    | (w !! 2) == "lose" = -(read (w !! 3))
    where w = words s

measureHappiness :: [NeighborConfiguration] -> (Person, Person) -> Happiness
measureHappiness c (a, b) = sum $ map (\(_, _, h) -> h) $ filter (\(x, y, _) -> (x `elem` (a:b:[])) && (y `elem` (a:b:[]))) c 

measureTableHappiness :: [NeighborConfiguration] -> SeatingArrangement -> Happiness
measureTableHappiness c = measureArrangementHappiness c . prepareArrangementToMeasure

prepareArrangementToMeasure :: SeatingArrangement -> SeatingArrangement
prepareArrangementToMeasure s = s ++ ((head s):[])

measureArrangementHappiness :: [NeighborConfiguration] -> SeatingArrangement -> Happiness
measureArrangementHappiness c (a:b:rest) = (measureHappiness c (a, b)) + (measureArrangementHappiness c (b:rest))
measureArrangementHappiness _ _ = 0

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let strings = lines input
    let neighbors = map getConfigurations strings
    let people = getAllPeople neighbors
    let possibleArrangements = permutations people
    print $ maximum $ map (measureTableHappiness neighbors) possibleArrangements
