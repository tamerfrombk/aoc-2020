# AOC 2020

This repo contains the solutions I used to solve the [Advent Of Code](https://adventofcode.com/2020) puzzles.

### Building
Each directory inside of `src` corresponds to a AOC day containing the solutions to that day's puzzles.
Each executable module is named `aoc2020-<day_number>` where `<day_number>` is the day of the AoC.

To build a specific day's solution:

```sh
cabal build aoc2020-<day_number>
```

OR

to build all days at once:

```sh
cabal build
```

### Running

All executables follow the same convention: they read all of input from stdin at once and require a single numeric argument 
describing which puzzle to solve. For example, assuming that the input is in a file called `input.txt`, to solve the 
1st problem of the 2nd day, run:

```sh
cabal run aoc2020-2 -- 1 < input.txt
```
