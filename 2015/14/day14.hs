import System.Environment
import Data.List
import Data.List.Split
import Control.Monad

type Time     = Int
type Speed    = Int
type Distance = Int 

data Currently = Flying | Resting deriving (Eq, Show)

type Reindeer   = (String, (Speed, Time), Time)
type Competitor = (Reindeer, Currently, Time, Distance, Int)

simulateOneStep :: Competitor -> Competitor
simulateOneStep ((n, (sp, ft), rt), c, t, d, sc)
    | c == Flying && t < ft  = ((n, (sp, ft), rt), c, t + 1, d + sp, sc)
    | c == Flying && t >= ft = ((n, (sp, ft), rt), Resting, 1, d, sc)   
    | c == Resting && t < rt  = ((n, (sp, ft), rt), c, t + 1, d, sc)
    | c == Resting && t >= rt = ((n, (sp, ft), rt), Flying, 1, d + sp, sc)   

simulateReindeer :: Reindeer -> Time -> Distance
simulateReindeer (name, (flightSpeed, flightTime), restTime) timeRemaining
    | timeRemaining > (flightTime + restTime) = (flightTime * flightSpeed) + (simulateReindeer (name, (flightSpeed, flightTime), restTime) (timeRemaining - (flightTime + restTime)))  
    | timeRemaining > flightTime              = (flightTime * flightSpeed)
    | timeRemaining < flightTime              = (flightSpeed * timeRemaining)
    | otherwise                               = 0    

makeReindeer :: String -> Reindeer
makeReindeer s = (w !! 0, (read (w !! 3), read (w !! 6)), read (w !! 13))
    where w = words s

makeCompetitor :: Reindeer -> Competitor
makeCompetitor r = (r, Flying, 0, 0, 0)

determineLeaders :: [Competitor] -> [String]
determineLeaders = map (\((n, _, _), _, _, _, _) -> n) . last . groupBy (\(_, _, _, d1, _) (_, _, _, d2, _) -> d1 == d2) . sortBy (\(_, _, _, d1, _) (_, _, _, d2, _) -> compare d1 d2)  

awardLeader :: [Competitor] -> [Competitor]
awardLeader competitors = map (\((n, a, b), c, d, e, sc) -> if n `elem` leaderNames then ((n, a, b), c, d, e, sc + 1) else ((n, a, b), c, d, e, sc)) competitors
    where leaderNames = determineLeaders competitors

simulateCompetition :: Time -> [Competitor] -> [Competitor]
simulateCompetition t c = if t > 0
    then simulateCompetition (t - 1) (awardLeader $ (map simulateOneStep c))
    else c

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let reindeer = map makeReindeer $ lines input
    let competitors = map makeCompetitor reindeer
    print $ maximum $ map (\r -> simulateReindeer r 2503) reindeer 
    print $ last $ sortBy (\(_, _, _, _, s1) (_, _, _, _, s2) -> compare s1 s2) $ simulateCompetition 2503 competitors
