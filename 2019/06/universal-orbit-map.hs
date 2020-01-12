import Prelude hiding (lookup)
import Data.List (intersect, elemIndex)
import Data.Map (Map, insert, keys, empty, lookup)

type Object = String

data Orbit = Orbit { orbiting :: Object
                   , orbiter  :: Object
                   } deriving (Eq, Show)  
                    
type OrbitMap = Map Object Object

parseOrbit :: String -> Orbit
parseOrbit string = Orbit orbitingObject orbiterObject
    where orbitingObject = takeWhile (/= ')') string
          orbiterObject  = tail $ dropWhile (/= ')') string 

addOrbit :: OrbitMap -> Orbit -> OrbitMap
addOrbit orbits orbit = insert (orbiter orbit) (orbiting orbit) orbits

makeOrbitMap :: [Orbit] -> OrbitMap
makeOrbitMap = foldl addOrbit empty

countDirectOrbits :: OrbitMap -> Int
countDirectOrbits = length . keys

countIndirectOrbit :: OrbitMap -> Object -> Int
countIndirectOrbit orbits object = maybe 0
                                         (\orbiting -> if orbiting == "COM"
                                                       then 0
                                                       else 1 + countIndirectOrbit orbits orbiting)
                                         (lookup object orbits)

countIndirectOrbits :: OrbitMap -> Int
countIndirectOrbits orbits = sum $ map (countIndirectOrbit orbits) objects
    where objects = keys orbits

checksum :: OrbitMap -> Int
checksum orbits = countIndirectOrbits orbits + countDirectOrbits orbits

orbitPath :: OrbitMap -> Object -> [Object] -> [Object]
orbitPath orbits object path = maybe path
                                     (\orbiting -> orbitPath orbits orbiting $ path ++ [orbiting])
                                     (lookup object orbits)

countJumpsBetween :: OrbitMap -> Object -> Object -> Maybe Int
countJumpsBetween orbits a b = let pathA  = orbitPath orbits a []
                                   pathB  = orbitPath orbits b []
                                   common = head $ intersect pathA pathB
                                   jumpsA = elemIndex common pathA
                                   jumpsB = elemIndex common pathB
                               in (+) <$> jumpsA <*> jumpsB 

main :: IO ()
main = do orbitMap <- makeOrbitMap . map parseOrbit . lines <$> readFile "input.txt" 
          putStr "Part 1: "  
          print $ checksum orbitMap
          putStr "Part 2: "
          case countJumpsBetween orbitMap "YOU" "SAN" of
               Just jumps -> print jumps
               Nothing -> error "No jump path between YOU and SAN"                    
