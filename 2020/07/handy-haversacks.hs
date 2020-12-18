import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Maybe (maybe)
import qualified Data.Map.Strict as Map

type Color = String
type Adjective = String
type Variety = (Adjective, Color)

data BagRule = BagRule
    { variety  :: Variety
    , contains :: Map.Map Variety Int
    } deriving Show

data RuleBook = RuleBook
    { rules :: Map.Map Variety (Map.Map Variety Int) 
    } deriving Show

parseVariety :: String -> Variety
parseVariety string =  
    let adjective:color:[] = take 2 $ words string
    in (adjective, color)

parseContainPair :: [String] -> (Variety, Int)
parseContainPair (number:adjective:color:_:[]) = ((adjective, color), read number)

parseContainPairs :: String -> [(Variety, Int)]
parseContainPairs =
    map parseContainPair . chunksOf 4 . words . drop 1 . dropWhile (/= ' ')

parseContains :: String -> Map.Map Variety Int
parseContains string =
    let containsString = intercalate " " . dropWhile (/= "contain") $ words string 
    in if containsString == "contain no other bags."
       then Map.empty
       else Map.fromList $ parseContainPairs containsString

parseBagRule :: String -> BagRule
parseBagRule string =
    let bagVariety  = parseVariety string
        containsMap = parseContains string
    in BagRule bagVariety containsMap

makeRuleBook :: [BagRule] -> RuleBook
makeRuleBook = RuleBook . Map.fromList . map (\bagRule -> (variety bagRule, contains bagRule))

canContain :: RuleBook -> Variety -> Variety -> Bool
canContain ruleBook toFind startingWith =
    let maybeRules = Map.lookup startingWith $ rules ruleBook
    in maybe False (\bagRules -> let number = Map.findWithDefault 0 toFind bagRules
                                 in number > 0
                   ) maybeRules

searchNested :: (Variety -> Bool) -> Map.Map Variety Int -> Bool
searchNested search rules = or . map search $ Map.keys rules

canContainNested :: RuleBook -> Variety -> Variety -> Bool
canContainNested ruleBook toFind startingWith =
    canContain ruleBook toFind startingWith ||
    let maybeRules = Map.lookup startingWith $ rules ruleBook
        nestedSearch = canContainNested ruleBook toFind
    in maybe False (searchNested nestedSearch) maybeRules

countRequiredBags :: RuleBook -> Variety -> Int
countRequiredBags ruleBook variety =
    let nested = Map.findWithDefault Map.empty variety $ rules ruleBook
    in sum $ Map.mapWithKey (\v n -> n + (n * (countRequiredBags ruleBook v))) nested

main :: IO ()
main = do
    ruleBook <- makeRuleBook . map parseBagRule . lines <$> readFile "input.txt"
    let searchForGold = canContainNested ruleBook ("shiny", "gold")
    putStr "Part 1: "
    print . length . filter id . map searchForGold . Map.keys $ rules ruleBook 
    putStr "Part 2: "
    print $ countRequiredBags ruleBook ("shiny", "gold")
