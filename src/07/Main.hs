import System.Environment ( getArgs )
import qualified Data.Map as M

data Bag = Bag {
    description :: String,
    children :: [(String, Int)]
} deriving Show

instance Eq Bag where
    b1 == b2 = (description b1) == (description b2)

type Cache = M.Map String Bag

makeCache :: String -> Cache
makeCache = foldr (\b m -> M.insert (description b) b m) M.empty . map (makeBag . words) . lines

findBag :: Cache -> String -> Bag
findBag cache desc = case M.lookup desc cache of
    Just x -> x
    Nothing -> error $ "bag " ++ desc ++ " is supposed to be in the cache"

hasBagOf :: String -> Bag -> Cache -> Bool
hasBagOf desc (Bag d cs) cache
    | d == desc = True
    | otherwise = any (flip (hasBagOf desc) cache . findBag cache . fst) cs

makeBag :: [String] -> Bag
makeBag ws = let
    desc = take 3 ws
    rest = drop 4 ws
    in Bag (makeDescription desc) (makeChildren rest)
    where makeDescription :: [String] -> String
          makeDescription = unwords . take 2

          makeChildren :: [String] -> [(String, Int)]
          makeChildren ["no","other","bags."] = []
          makeChildren (n:desc)               = (makeDescription desc, read n) : makeChildren (drop 3 desc)
          makeChildren []                     = []

solve1 :: String -> String
solve1 s = show . (\x -> x - 1) . sum . map (fromEnum . hasGolden) $ M.elems $ cache
    where   hasGolden :: Bag -> Bool
            hasGolden b = hasBagOf "shiny gold" b cache

            cache :: Cache
            cache = makeCache s

bagCount :: Bag -> Cache -> Int
bagCount (Bag d cs) cache = sum $ map go cs
    where   go :: (String, Int) -> Int
            go (s,n) = n + (n * count (findBag cache s))

            count :: Bag -> Int
            count = (flip bagCount) cache

goldenCount :: Cache -> Int
goldenCount cache = bagCount (findBag cache "shiny gold") cache

solve2 :: String -> String
solve2 = show . goldenCount . makeCache

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs
