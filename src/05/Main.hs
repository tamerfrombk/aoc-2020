import System.Environment ( getArgs )

move :: Char -> (Int, Int) -> (Int, Int)
move c (l, u)
    | c == 'F' || c == 'L' = (l, l + mid)
    | c == 'B' || c == 'R' = if even (u - l) then (l + mid, u) else (l + mid + 1, u)
    | otherwise            = error "undefined"
    where mid = (u - l) `div` 2

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (\ x f -> f x) x fs

bsp :: (Int, Int) -> String -> Int
bsp i s = let fns = map move s in fst $ applyAll fns i

row :: String -> Int
row s = bsp (0, 127) (take 7 s)

col :: String -> Int
col s = bsp (0, 7) (drop 7 s)

boardingId :: String -> Int
boardingId s = (row s * 8) + col s

solve1 :: String -> String
solve1 = show . maximum . map boardingId . lines

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

