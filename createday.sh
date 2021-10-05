#!/usr/bin/env bash

if [ "$#" -ne "1" ]; then
    echo "day name must be supplied"
    exit 1
fi

USER_DAY="$1"
DAY="0${USER_DAY}"
DAY_DIR="./src/${DAY}"
DAY_MAIN="${DAY_DIR}/Main.hs"
DAY_INPUT="${DAY_DIR}/input.txt"

set -e

mkdir -p $DAY_DIR
touch $DAY_INPUT

echo -e "\
import System.Environment ( getArgs )

solve1 :: String -> String
solve1 = undefined

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs (\"1\":_) = interact solve1 >> putChar '\\\\n'
mainWithArgs (\"2\":_) = interact solve2 >> putChar '\\\\n'
mainWithArgs _       = error \"invalid argument\"

main :: IO ()
main = getArgs >>= mainWithArgs
" > $DAY_MAIN

echo -e "\
executable aoc2020-$USER_DAY
    main-is:        Main.hs
    
    build-depends:    base ^>=4.14.1.0
    hs-source-dirs:   src/$DAY
    default-language: Haskell2010

" >> aoc2020.cabal

git add .
