import System.Environment ( getArgs )
import Data.Function ( on )

data Instruction = Acc Int Int
    | Jump Int Int
    | Nop Int Int
    deriving Show

line :: Instruction -> Int
line (Acc  l  _ ) = l
line (Jump l  _ ) = l
line (Nop  l  _ ) = l

arg :: Instruction -> Int
arg (Acc  _  a ) = a
arg (Jump _  a ) = a
arg (Nop  _  a ) = a

instance Eq Instruction where
    (==) = (==) `on` line

data Program = Program {
    ix      :: [Instruction]
    , accum :: Int
    , pc    :: Int
} | Done Int deriving Show

acc :: Program -> Int -> Program
acc (Program ix a pc) delta = Program ix (a + delta) (pc + 1)

jump :: Program -> Int -> Program
jump (Program ix a pc) offset = Program ix a (pc + offset)

nop :: Program -> Program
nop (Program ix a pc) = Program ix a (pc + 1)

fetch :: Program -> Int -> Instruction
fetch p line = ix p !! (line - 1)

put :: Program -> Instruction -> Program
put (Program ix acc pc) i = Program (i:ix) acc pc

exe :: Program -> Instruction -> Program
exe p (Acc  _ v) = acc p v
exe p (Jump _ v) = jump p v
exe p (Nop  _ _) = nop p

step :: [Int] -> Program -> ([Int], Program)
step visited p
    | ip `elem` visited = (visited, Done a)
    | otherwise         = (line i : visited, exe p i)
    where ip = pc p
          i  = fetch p ip
          a  = accum p

stepN :: Int -> Program -> ([Int], Program)
stepN = go []
    where go xs 0 p = (xs, p)
          go xs n p = let (xs', p') = step xs p in go xs' (n - 1) p'

run :: Program -> Int
run = go []
    where go _ (Done l) = l
          go visited p  = let (visited', p') = step visited p
                          in go visited' p'

makeProgram :: String -> Program
makeProgram s = Program { ix = ixs, accum = 0, pc = 1 }
    where ixs = map (uncurry makeInstruction) $ zip [1..] (lines s)

makeInstruction :: Int -> String -> Instruction
makeInstruction line s = go $ words s
    where go :: [String] -> Instruction
          go (name:arg:_)
            | name == "acc" = Acc  line (makeArg arg)
            | name == "jmp" = Jump line (makeArg arg)
            | name == "nop" = Nop  line 0 -- we don't care about the argument to nop
            | otherwise     = error $ name ++ " is not a valid instruction"
          go _              = error $ "invalid instruction syntax at line " ++ show line

          makeArg :: String -> Int
          makeArg (sign:n:rest)
            | n == '0'    = 0 -- inputs with (-0) and (+0) don't play nice with read
            | sign == '-' = negate $ read (n:rest)
            | sign == '+' = read (n:rest)
            | otherwise   = error $ "unable to make arg: " ++ show (sign:n:rest)

solve1 :: String -> String
solve1 = show . run . makeProgram

solve2 :: String -> String
solve2 = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs ("1":_) = interact solve1 >> putChar '\n'
mainWithArgs ("2":_) = interact solve2 >> putChar '\n'
mainWithArgs _       = error "invalid argument"

main :: IO ()
main = getArgs >>= mainWithArgs

