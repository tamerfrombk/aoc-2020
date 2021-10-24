import System.Environment ( getArgs )

preamble :: Int -> Int -> [Int] -> [Int]
preamble n i xs
    | i <= n    = take i xs
    | otherwise = take n $ drop (i - n) xs

isValid :: Int -> [Int] -> Bool
isValid n ns = any (\(a, b) -> a + b == n) $ pairs ns
    where pairs :: [Int] -> [(Int, Int)]
          pairs ns = [(x, y) | x <- ns, y <- ns, x < y]

toNums :: String -> [Int]
toNums = map (\x -> read x :: Int) . lines

sumsOf :: Int -> [Int] -> [[Int]]
sumsOf l xs = filter (\ns -> length ns == l)
    $ map (\(i, _) -> preamble l i xs)
    $ zip [0..] xs

firstInvalid :: [Int] -> Int
firstInvalid ns = fst $ head
    $ filter (not . uncurry isValid)
    $ filter (\(_, ns) -> length ns == 25)
    $ map (\(i, n) -> (n, preamble 25 i ns)) ins
    where ins = zip [0..] ns

solve1 :: String -> String
solve1 = show . firstInvalid . toNums

weakness :: [Int] -> Int
weakness ns = minimum ns + maximum ns

contiguousSumTo :: Int -> [Int] -> [Int]
contiguousSumTo target xs = head $ concat
    $ map (\vs -> filter (\v -> sum v == target) vs)
    $ filter (elem target . map sum)
    $ zipWith (\i _ -> sumsOf i xs) [2..length xs] xs

solve2 :: String -> String
solve2 = show  . weakness . contiguousSumTo 373803594 . toNums

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

