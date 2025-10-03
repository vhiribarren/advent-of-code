import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf, partition, find)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)

problemFilename :: String
problemFilename = "../inputs/day_10.txt"

type BotId = Int
type OutputId = Int
type ElementId = Int
data Target = OutputId OutputId | BotId BotId deriving (Show)
data Bot = Bot
  { botId :: BotId,
    botElements :: [ElementId],
    low, high :: Target
  }
  deriving (Show)
type Bots = IntMap Bot
type Outputs = IntMap [ElementId]

targetComboChip :: [ElementId]
targetComboChip = [61, 17]

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
-- putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . botId . runSimulationUntil targetComboChip . parse
solverProb2 = undefined

parse :: String -> Bots
parse input =
  let (graph, initVal) = partition ("bot" `isPrefixOf`) $ lines input
      bots = foldl buildBotGraph IntMap.empty (map words graph)
   in foldl injectInitVal bots (map words initVal)

buildBotGraph :: Bots -> [String] -> Bots
buildBotGraph bots w =
  let botId' = read $ w !! 1
      lowTarget = parseTarget (w !! 5) (w !! 6)
      highTarget = parseTarget (w !! 10) (w !! 11)
   in IntMap.insert botId' (Bot botId' [] lowTarget highTarget) bots

injectInitVal :: Bots -> [String] -> Bots
injectInitVal bots w =
  let val = read $ w !! 1
      botId' = read $ w !! 5
   in passValToBot val botId' bots

passValToBot :: ElementId -> BotId -> Bots -> Bots
passValToBot val = IntMap.update (\b -> Just (b {botElements = val : botElements b}))

parseTarget :: String -> String -> Target
parseTarget target id' = (if target == "bot" then BotId else OutputId) (read id')

runSimulationUntil :: [ElementId] -> Bots -> Bot
runSimulationUntil comboChip bots =
  let bot = traceShowId $ findBotWith2Chips bots
   in if isBotFound comboChip bot
      then bot
      else runSimulationUntil comboChip $ transferBotElements bot bots

isBotFound :: [ElementId] -> Bot -> Bool
isBotFound comboChip bot = all (`elem` botElements bot) comboChip

findBotWith2Chips :: Bots -> Bot
findBotWith2Chips = snd . fromJust . find (\(_, v) -> length (botElements v) == 2) . IntMap.toList

transferBotElements :: Bot -> Bots -> Bots
transferBotElements bot bots =
  let highVal = maximum $ botElements bot
      lowVal = minimum $ botElements bot
      lowTarget = low bot
      highTarget = high bot
      bots' = IntMap.update (\_ -> Just $ bot {botElements = []}) (botId bot) bots
      bots'' = case lowTarget of
        BotId i -> passValToBot lowVal i bots'
        _ -> bots'
      bots''' = case highTarget of
        BotId i -> passValToBot highVal i bots''
        _ -> bots''
   in bots'''