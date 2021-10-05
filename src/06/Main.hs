import System.Environment ( getArgs )
import Data.List ( sort, group, groupBy )

type Group = String

countYes :: Group -> Int
countYes = length . group . sort

makeGroups :: String -> [Group]
makeGroups = collect . lines
    where collect :: [String] -> [String]
          collect =  filter present . map concat . groupBy (\a b -> all present [a,b])
                where present = not . null

solve1 :: String -> String
solve1 = show . sum . map countYes . makeGroups

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

