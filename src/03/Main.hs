import System.Environment ( getArgs )

type Cell = Int
type Map = [[Cell]]
type Slope = (Int, Int)

height :: Map -> Int
height = length

width :: Map -> Int
width = length . head

toCell :: Char -> Cell
toCell c
    | c == '.'  = 0
    | c == '#'  = 1
    | otherwise = error "unknown cell value"

toMap :: String -> Map
toMap = map (map toCell) . lines

isBottom :: Map -> Int -> Int -> Bool
isBottom m r _ = r >= height m

isTree :: Map -> Int -> Int -> Bool
isTree m r c = (!!) ((!!) m r) (c `mod` width m) == 1

navigate :: Slope -> Map -> Int
navigate (dr,dc) m = navigate' m 0 0
    where navigate' m r c
            | isBottom m r c = 0
            | isTree m r c   = 1 + navigate' m (r + dr) (c + dc)
            | otherwise      = navigate' m (r + dr) (c + dc)

solve1 :: String -> String
solve1 = show . navigate (1, 3) . toMap

solve2 :: String -> String
solve2 input = show $ product [
        navigate (1, 1) m
        , navigate (1, 3) m
        , navigate (1, 5) m
        , navigate (1, 7) m
        , navigate (2, 1) m
    ]
    where m = toMap input

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

