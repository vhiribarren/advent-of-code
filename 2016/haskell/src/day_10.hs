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
type Outputs = IntMap ElementId
type Containers = (Bots, Outputs)

targetComboChip :: [ElementId]
targetComboChip = [61, 17]

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . botId . runSimulationUntil targetComboChip . parse
solverProb2 = show . multChip . snd . runFullSimulation . parse
  where multChip o = o IntMap.!0 * o IntMap.!1 * o IntMap.!2

parse :: String -> Containers
parse input =
  let (graph, initVal) = partition ("bot" `isPrefixOf`) $ lines input
      bots = foldl buildBotGraph IntMap.empty (map words graph)
   in (foldl injectInitVal bots (map words initVal), IntMap.empty)

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

runSimulationUntil :: [ElementId] -> Containers -> Bot
runSimulationUntil comboChip containers@(bots, _) =
  let bot = fromJust $ traceShowId $ findBotWith2Chips bots
   in if isBotFound comboChip bot
      then bot
      else runSimulationUntil comboChip $ transferBotElements bot containers

runFullSimulation :: Containers -> Containers
runFullSimulation containers@(bots, _) =
  let bot = traceShowId $ findBotWith2Chips bots
   in case bot of
        Nothing -> containers
        Just b -> runFullSimulation $ transferBotElements b containers

isBotFound :: [ElementId] -> Bot -> Bool
isBotFound comboChip bot = all (`elem` botElements bot) comboChip

findBotWith2Chips :: Bots -> Maybe Bot
findBotWith2Chips = fmap snd . find (\(_, v) -> length (botElements v) == 2) . IntMap.toList

transferBotElements :: Bot -> Containers -> Containers
transferBotElements bot (bots, outputs) =
  let highVal = maximum $ botElements bot
      lowVal = minimum $ botElements bot
      lowTarget = low bot
      highTarget = high bot
      bots' = IntMap.update (\_ -> Just $ bot {botElements = []}) (botId bot) bots
      (bots'', outputs'') = case lowTarget of
        BotId i -> (passValToBot lowVal i bots', outputs)
        OutputId i -> (bots', IntMap.insert i lowVal outputs)
      result = case highTarget of
        BotId i -> (passValToBot highVal i bots'', outputs'')
        OutputId i -> (bots'', IntMap.insert i highVal outputs'')
   in result