import System.Environment
import Data.List
import Data.List.Split
import Data.Char

type Molecule    = String
type Replacement = (String, String)

parseReplacement :: String -> Replacement
parseReplacement string = (head halves, last halves)
    where halves = splitOn " => " string

getMoleculesFromReplacement :: Molecule -> Replacement -> [Molecule]
getMoleculesFromReplacement m r = getMoleculesFromReplacement' m r 0 []

getMoleculesFromReplacement' :: Molecule -> Replacement -> Int -> [Molecule] -> [Molecule]
getMoleculesFromReplacement' m r n ms
    | n < length m && ((take (length $ fst r) $ drop n $ m) == fst r) = getMoleculesFromReplacement' m r (n + 1) (((take n $ m) ++ (snd r) ++ (drop (n + (length $ fst r)) m)):ms)
    | n < length m                                                    = getMoleculesFromReplacement' m r (n + 1) ms
    | otherwise                                                       = ms

getAllMolecules :: Molecule -> [Replacement] -> [Molecule]
getAllMolecules m = concat . map (getMoleculesFromReplacement m)

newGetAllMolecules :: Int -> Molecule -> [Replacement] -> [(Molecule, Int)]
newGetAllMolecules n m rs = zip (getAllMolecules m rs) (cycle [n + 1])

switch :: (a, b) -> (b, a)
switch (a, b) = (b, a)

getUnique :: (Eq a, Ord a) => [a] -> [a]
getUnique list = foldl unique (take 1 list) (tail list)
    where unique acc element = if element `elem` acc
                               then acc
                               else (element:acc)

findFirstMolecule :: Molecule -> [Replacement] -> Int
findFirstMolecule m rs = findFirstMolecule' m rs ["e"] 0

findFirstMolecule' :: Molecule -> [Replacement] -> [Molecule] -> Int -> Int 
findFirstMolecule' m rs ms n
    | m `elem` ms = n
    | otherwise   = findFirstMolecule' m rs (getUnique $ ((getAllMolecules (head ms) rs) ++ tail ms)) (n + 1)

repeatUntilE :: Molecule -> [Replacement] -> Int
repeatUntilE m rs = repeatUntilE' [(m, 0)] rs

repeatUntilE' :: [(Molecule, Int)] -> [Replacement] -> Int
repeatUntilE' ms rs
    | not $ null $ filter (\(mol, _) -> mol == "e") ms = snd $ head $ filter (\(mol, _) -> mol == "e") ms
    | otherwise                                        = repeatUntilE' (newGetAllMolecules (snd $ head ms) (fst $ head ms) rs ++ tail ms) rs

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let replacements = map parseReplacement $ takeWhile (/= "") $ lines input
    let molecule = last $ lines input
    let switchedReplacements = map switch replacements
    print $ repeatUntilE molecule switchedReplacements
