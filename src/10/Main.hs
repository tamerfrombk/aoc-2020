{-# LANGUAGE TupleSections #-}

import System.Environment ( getArgs )
import Data.List ( sort, find )
import Data.Maybe ( maybeToList, fromMaybe )
import Control.Applicative ( (<|>) )

type Jolt = Int

findConnectable :: Jolt -> [Jolt] -> Maybe (Int, Jolt)
findConnectable j js = foldr1 (<|>) $ map toPair [1..3]
    where toPair d = (d,) <$> find (\x -> x - d == j) js

joltageDifferentials :: [Jolt] -> [(Int, Jolt)]
joltageDifferentials js = map (fromMaybe (3, deviceJoltage) . (`findConnectable` js)) (0:js)
    where deviceJoltage = 3 + maximum js

makeJolts :: String -> [Jolt]
makeJolts =  sort . map (\ x -> read x :: Jolt) . lines

solve1 :: String -> String
solve1 s = show $ diff 1 * diff 3
           where diff n = length $ filter (==n) jdd
                 jdd = map fst $ joltageDifferentials $ makeJolts s

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

