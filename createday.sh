#!/usr/bin/env bash

if [ "$#" -ne "1" ]; then
    echo "day name must be supplied"
    exit 1
fi

day="$1"

mkdir $day 
cd $day
touch input.txt

echo -e "\
import System.Environment ( getArgs )

solve1 :: String -> String
solve1 = undefined

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs (\"1\":_) = interact solve1 >> putChar '\\\\n'
mainWithArgs (\"2\":_) = interact solve2 >> putChar '\\\\n'
mainWithArgs _       = error \"non-valid argument\"

main :: IO ()
main = getArgs >>= mainWithArgs
" > Main.hs

git add ..