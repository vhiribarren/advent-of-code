import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read

problemFilename :: String
problemFilename = "../inputs/day_12.txt"

codePointerRegister :: String
codePointerRegister = "cp"

type Registers = Map String Int

registersInitP1, registersInitP2 :: Registers
registersInitP1 = Map.fromList [(codePointerRegister, 0), ("a", 0),("b", 0),("c", 0),("d", 0)]
registersInitP2 = Map.fromList [(codePointerRegister, 0), ("a", 0),("b", 0),("c", 1),("d", 0)]


main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . (Map.! "a") .runProgram registersInitP1
solverProb2 = show . (Map.! "a") .runProgram registersInitP2

runProgram :: Registers -> String -> Registers
runProgram r program =
  let instructions = map words $ lines program
      cpLine = getCodePointer r
      instLine = instructions !! cpLine
  in if cpLine < 0 || cpLine >= length instructions 
     then r
     else runProgram (applyInstruction r instLine) program

applyInstruction ::  Registers -> [String] -> Registers
applyInstruction r ["cpy", x, y] = let val = getValOrRegister r x in
  incCodePointer $ Map.insert y val r
applyInstruction r ["inc", x] = incCodePointer $ Map.adjust (+1) x r
applyInstruction r ["dec", x] = incCodePointer $ Map.adjust (flip (-) 1) x r
applyInstruction r ["jnz", x, y] = let val = getValOrRegister r x in
  if val /= 0
  then  Map.adjust (+ read y) codePointerRegister r
  else incCodePointer r
applyInstruction _ o = error ("This instruction cannot happen: " ++ unwords o)

incCodePointer :: Registers -> Registers
incCodePointer = Map.adjust (+1) codePointerRegister

getCodePointer :: Registers -> Int
getCodePointer = flip (Map.!) codePointerRegister

getValOrRegister :: Registers -> String -> Int
getValOrRegister r x = case readMaybe x of
  Nothing -> r Map.! x
  Just val -> val