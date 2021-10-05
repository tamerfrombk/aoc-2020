# AOC 2020

This repo contains the solutions I used to solve the [Advent Of Code](https://adventofcode.com/2020) puzzles.

### Layout

Each directory inside of `src` corresponds to a AoC day. Inside of each of those directories is the input to that day's puzzles as well as a `Main.hs` file implementing all of that day's puzzles.

Each of these day directories is set up as a separate cabal executable build following this naming convention: `aoc2020-<day_number>` where `<day_number>` is the day of the AoC.

### Building

To build a specific day's solution:

```sh
cabal build aoc2020-<day_number>
```

To build all days at once:

```sh
cabal build
```

### Running

All executables follow the same convention: they read all of input from stdin at once and require a single numeric argument describing which puzzle on that day to solve. For example, assuming that the input is in a file called `input.txt`, to solve the
1st problem of the 2nd day, run:

```sh
cabal run aoc2020-2 -- 1 < input.txt
```
