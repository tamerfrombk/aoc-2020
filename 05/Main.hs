import System.Environment ( getArgs )
data BoardingPass = BoardingPass {
    rowV :: String,
    colV :: String
} deriving Show

toBoardingPass :: String -> BoardingPass
toBoardingPass s = BoardingPass { rowV = take 7 s, colV = drop 7 s }

middle :: Fractional a => a -> a -> a
middle lo hi = lo + ((hi - lo) / 2)

calculateRow :: BoardingPass -> Int
calculateRow (BoardingPass rows _) = calculateRow' rows a b
    where calculateRow' [r]      mi mx = if r == 'F' then mi else mx
          calculateRow' ('F':rs) mi mx = calculateRow' rs mi $ floor $ middle mi mx
          calculateRow' ('B':rs) mi mx = calculateRow' rs (ceiling (middle mi mx)) mx
          a = 0 :: Float
          b = 127 :: Float

calculateCol :: BoardingPass -> Int
calculateCol _ = 7

seatId :: BoardingPass -> Int
seatId p = calculateRow p * 8 + calculateCol p 

solve1 :: String -> String
solve1 = show . maximum . map (seatId . toBoardingPass) . lines

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

