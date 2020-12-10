# AOC 2020

This repo contains the solutions I used to solve the [Advent Of Code](https://adventofcode.com/2020) puzzles.

### Building
Each directory corresponds to a AOC day containing the solutions to that day's puzzles.

To build a specific day:

```sh
# build day one
make 01
```

OR

```sh
# build everything at once
make
```

This will generate an executable in the day's directory with the same name as the day. For example, day 1's directory is `01` and the executable is called `01`.

### Running

All executables follow the same convention: they read all of input from stdin at once and require a single numeric argument describing which puzzle to solve. For example, assuming that the input is in a file called `input.txt`, to solve the 1st problem of the 2nd day, run:

```sh
./02 1 < input.txt
```
