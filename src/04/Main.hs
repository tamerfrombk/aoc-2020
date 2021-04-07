{-# LANGUAGE TupleSections #-}

import System.Environment ( getArgs )
import Data.Char ( isDigit )

type Validator = (String -> Bool)
type Passport = [(String, String)]

tautology :: a -> Bool
tautology _ = True

isBetween :: Ord a => a -> a -> a -> Bool
isBetween mi mx v = v >= mi && v <= mx

validators1 :: [(String, Validator)]
validators1 = map (, tautology) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

validators2 :: [(String, Validator)]
validators2 = [("byr", vByr)
    , ("iyr", vIyr)
    , ("eyr", vEyr)
    , ("hgt", vHgt)
    , ("hcl", vHcl)
    , ("ecl", vEcl)
    , ("pid", vPid)
    , ("cid", tautology)
    ]
    where vByr x = let v = read x in length x == 4 && isBetween 1920 2002 v
          vIyr x = let v = read x in length x == 4 && isBetween 2010 2020 v
          vEyr x = let v = read x in length x == 4 && isBetween 2020 2030 v
          vHgt x = Just True == (f <$> readHeight x)
                   where f (n, "cm") = isBetween 150 193 n
                         f (n, "in") = isBetween 59 76 n
                         f _         = False
                         readHeight xs = let (n, u) = span isDigit xs 
                                         in if u == "cm" || u == "in" 
                                            then Just (read n, u)
                                            else Nothing
          vHcl (x:xs) = x == '#' && length xs == 6 && all (\n -> isDigit n || n `elem` ['a'..'f']) xs
          vHcl _      = False
          vEcl x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          vPid x = length x == 9 && all isDigit x


isValidField :: Passport -> [(String, Validator)] -> String -> Bool
isValidField _  _ "cid"               = True
isValidField passport validators key  = Just True == (lookup key validators <*> lookup key passport)

isValid :: Passport -> [(String, Validator)] -> Bool
isValid p validators = all (isValidField p validators) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

toPassports :: [String] -> [Passport]
toPassports [] = []
toPassports xs = let (passport, rest) = break null xs
                 in toPassport passport : toPassports (tail' rest)
                 where toPassport = map (\word -> (takeWhile (/=':') word, tail $ dropWhile (/=':') word)) . words . unlines

solve :: [(String, Validator)] -> String -> String
solve vs = show . length . filter (`isValid` vs) . toPassports . lines

solve1 :: String -> String
solve1 = solve validators1

solve2 :: String -> String
solve2 = solve validators2

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

