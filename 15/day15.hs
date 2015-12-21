import System.Environment
import Data.List
import Data.List.Split

data Ingredient = Ingredient { name :: String
                             , capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             } deriving (Show)

type Recipe = [(Ingredient, Int)]

scoreProperty :: (Ingredient -> Int) -> Ingredient -> Int -> Int
scoreProperty property ingredient amount = (property ingredient) * amount

scoreProperties :: Ingredient -> Int -> [Int]
scoreProperties ingredient amount =  map (\property -> scoreProperty property ingredient amount) $ (capacity:durability:flavor:texture:[])

countCalories :: Ingredient -> Int -> Int
countCalories ingredient amount = scoreProperty calories ingredient amount

scoreRecipe :: Recipe -> Int
scoreRecipe = product . map (\n -> if n > 0 then n else 0) . foldl1 (zipWith (+)) . map (\(i, a) -> scoreProperties i a)

countRecipeCalories :: Recipe -> Int
countRecipeCalories = sum . map (\(i, a) -> countCalories i a)

parseIngredient :: String -> Ingredient
parseIngredient s = Ingredient name (read $ values !! 0) (read $ values !! 1) (read $ values !! 2) (read $ values !! 3) (read $ values !! 4)
    where name   = head $ splitOn ": " s
          values = map last $ map (splitOn " ") $ splitOn ", " $ last $ splitOn ": " s

possibleCombinations = [x:y:z:w:[] | x <- [0..100], y <- [0..100], z <- [0..100], w <- [0..100], (x + y + z + w) == 100]
testPossibleCombinations = [x:y:[] | x <- [0..100], y <- [0..100], (x + y) == 100]

makeRecipe :: [Ingredient] -> [Int] -> Recipe
makeRecipe ingredients amounts = zip ingredients amounts

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let ingredients = map parseIngredient $ lines input
    print $ maximum $ map scoreRecipe $ filter (\r -> countRecipeCalories r == 500) $ map (makeRecipe ingredients) $ possibleCombinations
