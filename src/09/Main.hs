import System.Environment ( getArgs )

preamble :: Int -> Int -> [Int] -> [Int]
preamble n i xs
    | i <= n    = take i xs
    | otherwise = take n $ drop (i - n) xs

preamble5, preamble25 :: Int -> [Int] -> [Int]
preamble5  = preamble 5
preamble25 = preamble 25

isValid :: Int -> [Int] -> Bool
isValid n ns = any (\(a, b) -> a + b == n) $ pairs ns
    where pairs :: [Int] -> [(Int, Int)]
          pairs ns = [(x, y) | x <- ns, y <- ns, x < y]

toNums :: String -> [Int]
toNums = map (\x -> read x :: Int) . lines

solve1 :: String -> String
solve1 s = show $ fst $ head
    $ filter (not . uncurry isValid)
    $ filter (\(_, ns) -> length ns == 25)
    $ map (\(i, n) -> (n, preamble25 i ns)) ins
    where ins = zip [0..] ns
          ns  = toNums s

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

