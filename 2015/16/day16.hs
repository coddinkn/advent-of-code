import System.Environment
import Data.List.Split
import Data.Maybe

data Aunt = Aunt { number :: Int
                 , children :: Maybe Int
                 , cats :: Maybe Int
                 , samoyeds :: Maybe Int
                 , pomeranians :: Maybe Int
                 , vizslas :: Maybe Int
                 , akitas :: Maybe Int
                 , goldfish :: Maybe Int
                 , trees :: Maybe Int
                 , cars :: Maybe Int
                 , perfumes :: Maybe Int
                 } deriving (Show)

makeAuntList :: Aunt -> [Maybe Int]
makeAuntList a = map (\v -> v a) [children, cats, samoyeds, pomeranians, vizslas, akitas, goldfish, trees, cars, perfumes] 

adjustForMFCSAM :: Aunt -> Aunt -> Bool
adjustForMFCSAM a b = adjustForMFCSAM' 0 (makeAuntList a) (makeAuntList b)

adjustForMFCSAM' :: Int -> [Maybe Int] -> [Maybe Int] -> Bool
adjustForMFCSAM' n ((Just x):xs) ((Just y):ys)
    | n == 1 = y > x && (adjustForMFCSAM' (n + 1) xs ys)
    | n == 7 = y > x && (adjustForMFCSAM' (n + 1) xs ys)
    | n == 3 = y < x && (adjustForMFCSAM' (n + 1) xs ys)
    | n == 6 = y < x && (adjustForMFCSAM' (n + 1) xs ys)
    | otherwise = y == x && (adjustForMFCSAM' (n + 1) xs ys)
adjustForMFCSAM' n (x:xs) (y:ys) = True && (adjustForMFCSAM' (n + 1) xs ys)
adjustForMFCSAM' n [] [] = True

maybeEq :: (Eq a) => Maybe a -> Maybe a -> Bool
maybeEq Nothing Nothing = True
maybeEq _ Nothing = True
maybeEq Nothing _ = True
maybeEq (Just x) (Just y) = (x == y)

instance Eq Aunt where
    x == y = and $ map (\(a, b) -> maybeEq a b) $ zip (makeAuntList x) (makeAuntList y)

setAuntProp :: Aunt -> (String, Int) -> Aunt
setAuntProp (Aunt num child cat samo pomer vizs akita gold tree car perf) (p, nv)
    | p == "Sue"         = Aunt nv child cat samo pomer vizs akita gold tree car perf
    | p == "children"    = Aunt num v cat samo pomer vizs akita gold tree car perf
    | p == "cats"        = Aunt num child v samo pomer vizs akita gold tree car perf
    | p == "samoyeds"    = Aunt num child cat v pomer vizs akita gold tree car perf
    | p == "pomeranians" = Aunt num child cat samo v vizs akita gold tree car perf
    | p == "vizslas"     = Aunt num child cat samo pomer v akita gold tree car perf
    | p == "akitas"      = Aunt num child cat samo pomer vizs v gold tree car perf
    | p == "goldfish"    = Aunt num child cat samo pomer vizs akita v tree car perf
    | p == "trees"       = Aunt num child cat samo pomer vizs akita gold v car perf
    | p == "cars"        = Aunt num child cat samo pomer vizs akita gold tree v perf
    | p == "perfumes"    = Aunt num child cat samo pomer vizs akita gold tree car v
    | otherwise          = Aunt num child cat samo pomer vizs akita gold tree car perf
    where v = (Just nv)

nothingAunt = Aunt (-1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

makeAunt :: String -> Aunt
makeAunt s = makeAunt' nothingAunt sList
    where sList = splitOn " " $ filter (\c -> not $ c `elem` ",:") s

makeAunt' :: Aunt -> [String] -> Aunt
makeAunt' a (p:v:rest) = makeAunt' (setAuntProp a (p, read v)) rest
makeAunt' a _ = a

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let aunts = map makeAunt $ lines input
    let thankYouSue = Aunt 0 (Just 3) (Just 7) (Just 2) (Just 3) (Just 0) (Just 0) (Just 5) (Just 3) (Just 2) (Just 1)   
    mapM_ print $ filter (adjustForMFCSAM thankYouSue) aunts
