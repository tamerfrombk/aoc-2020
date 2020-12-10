import System.Environment

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 
mainWithArgs ("2":_) = interact solve2
mainWithArgs _       = error "non-valid argument"

solve1 :: String -> String
solve1 input = show $ f $ map read $ lines input 
        where f xs = head [x*y | x <- xs, y <- xs, x + y == 2020]

solve2 :: String -> String
solve2 input = show $ f $ map read $ lines input 
        where f xs = head [x*y*z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
