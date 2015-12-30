import System.Environment
import System.Random.Shuffle
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

repeatUntilE :: Molecule -> [Replacement] -> Int
repeatUntilE m rs = repeatUntilE' [(m, 0)] rs

repeatUntilE' :: [(Molecule, Int)] -> [Replacement] -> Int
repeatUntilE' ms rs
    | not $ null $ filter (\(mol, _) -> mol == "e") ms = snd $ head $ filter (\(mol, _) -> mol == "e") ms
    | otherwise                                        = repeatUntilE' (newGetAllMolecules (snd $ head ms) (fst $ head ms) rs ++ tail ms) rs

repeatUntilEM :: Int -> [(Molecule, Int)] -> [Replacement] -> IO Int
repeatUntilEM n ms rs
    | not $ null $ filter (\(mol, _) -> mol == "e") ms = return $ snd $ head $ filter (\(mol, _) -> mol == "e") ms
    | n < 1000                                         = return intermediateResult >>= (\mols -> repeatUntilEM (n + 1) mols rs)
    | otherwise                                        = shuffleM intermediateResult >>= (\mols -> repeatUntilEM 0 mols rs)
    where intermediateResult = newGetAllMolecules (snd $ head ms) (fst $ head ms) rs ++ tail ms
    
main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let replacements = map parseReplacement $ takeWhile (/= "") $ lines input
    let molecule = last $ lines input
    let switchedReplacements = map switch replacements
    shuffled <- shuffleM switchedReplacements
    answer <- repeatUntilEM 0 [(molecule, 0)] switchedReplacements
    print answer
