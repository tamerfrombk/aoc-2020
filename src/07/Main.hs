import System.Environment ( getArgs )
import Data.Char ( isDigit )

data Bag = Bag {
    description :: String,
    children :: [(String, Int)]
} deriving Show

instance Eq Bag where
    b1 == b2 = (description b1) == (description b2)

digit :: String -> Int
digit s = if isDigit $ head s
          then read s
          else error (s ++ " is not a digit")

findBag :: [Bag] -> String -> Bag
findBag (b:bs) desc
    | desc == description b = b
    | otherwise             = findBag bs desc
findBag _    []             = error "bag must be here"

removeBag :: Bag -> [Bag] -> [Bag]
removeBag b (bb:bs)
    | b == bb   = bs
    | otherwise = bb : removeBag b bs
removeBag _ []  = []

hasBagOf :: String -> Bag -> [Bag] -> Bool
hasBagOf desc bag cache
    | description bag == desc = True
    | (null . children) bag   = False
    | otherwise               = any (\b -> hasBagOf desc b cache) $ map (findBag cache) $ map fst $ children bag

makeBag :: [String] -> Bag
makeBag ws = let
    desc = take 3 ws
    rest = drop 4 ws
    in Bag (makeDescription desc) (makeChildren rest)
    where makeDescription :: [String] -> String
          makeDescription = unwords . take 2

          makeChildren :: [String] -> [(String, Int)]
          makeChildren ["no","other","bags."] = []
          makeChildren (n:desc)               = (makeDescription desc, digit n) : makeChildren (drop 3 desc)
          makeChildren []                     = []

solve1 :: String -> String
solve1 s = show . (\x -> x - 1) . sum . map boolToInt . map hasGolden $ cache
         where  hasGolden :: Bag -> Bool
                hasGolden b = hasBagOf "shiny gold" b cache

                boolToInt :: Bool -> Int
                boolToInt x = if x == True then 1 else 0

                cache :: [Bag]
                cache = map (makeBag . words) $ lines s


solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs
