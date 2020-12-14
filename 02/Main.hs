import System.Environment ( getArgs )

type Input = (Int, Int, Char, String)

toInput :: String -> Input
toInput =  makeInput . words
    where makeInput (range:value:pwd:_) = (mi, mx, head value, pwd)
            where mi = read $ takeWhile (/='-') range
                  mx = read $ tail $ dropWhile (/='-') range
          makeInput _                   = error "can't parse input"

isValid1 :: Input -> Bool
isValid1 (mi, mx, c, pwd) = let count = length $ filter (==c) pwd
                            in count >= mi && count <= mx

solve1 :: String -> String
solve1 = show . length . filter isValid1 . map toInput . lines

isValid2 :: Input -> Bool
isValid2 (p1, p2, c, pwd) = (x == c && y /= c) || (x /= c && y == c)
    where x = (!!) pwd (p1 - 1)
          y = (!!) pwd (p2 - 1)

solve2 :: String -> String
solve2 = show . length . filter isValid2 . map toInput . lines

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs