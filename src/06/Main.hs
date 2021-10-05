import System.Environment ( getArgs )
import Data.List ( sort, group, groupBy )

type Group = (String, Int)

countYes :: Group -> Int
countYes = length . group . sort . fst

makeGroups :: String -> [Group]
makeGroups = collect . lines
    where collect :: [String] -> [Group]
          collect = filter (\(_, l) -> l > 0) . groups . groupBy (\a b -> all present [a,b])
                where present = not . null
                      groups (y:ys) = (concat y, length y) : groups ys
                      groups []     = []

solve1 :: String -> String
solve1 = show . sum . map countYes . makeGroups

countAllYes :: Group -> Int
countAllYes (s, l) = length . filter (\g -> length g == l ) $ group $ sort s

solve2 :: String -> String
solve2 = show . sum . map countAllYes . makeGroups

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

