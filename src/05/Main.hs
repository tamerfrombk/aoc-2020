import System.Environment ( getArgs )
import Data.List ( sort )

move :: Char -> (Int, Int) -> (Int, Int)
move c (l, u)
    | c == 'F' || c == 'L' = (l, l + mid)
    | c == 'B' || c == 'R' = if even (u - l) then (l + mid, u) else (l + mid + 1, u)
    | otherwise            = error "undefined"
    where mid = (u - l) `div` 2

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (flip ($)) x fs

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
solve2 = show . diff2 . sort . map boardingId . lines
    where diff2 (x:y:xs) = if y - x == 2 then x + 1 else diff2 xs
          diff2 [x]      = x
          diff2 []       = error "should never happen"

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

