import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf, partition, find)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import Control.Monad.State
import Data.Bifunctor (first, second)

problemFilename :: String
problemFilename = "../inputs/day_10.txt"

newtype BotId = BotId Int deriving (Show, Ord, Eq)
newtype OutputId = OutputId Int deriving (Show, Ord, Eq)
type ElementId = Int
data Target = OutputTarget OutputId | BotTarget BotId deriving (Show)
data Bot = Bot
  { botId :: BotId,
    botElements :: [ElementId],
    low, high :: Target
  }
  deriving (Show)
type Bots = Map BotId Bot
type Outputs = Map OutputId ElementId
type Containers = (Bots, Outputs)
type BotState = State Containers

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
  where multChip o = product $ (o Map.!) <$> [OutputId 0, OutputId 1, OutputId 2]

parse :: String -> Containers
parse input =
  let (graph, initVal) = partition ("bot" `isPrefixOf`) $ lines input
      bots = foldl buildBotGraph Map.empty (map words graph)
   in (foldl injectInitVal bots (map words initVal), Map.empty)

buildBotGraph :: Bots -> [String] -> Bots
buildBotGraph bots w =
  let botId' = BotId $ read $ w !! 1
      lowTarget = parseTarget (w !! 5) (w !! 6)
      highTarget = parseTarget (w !! 10) (w !! 11)
   in Map.insert botId' (Bot botId' [] lowTarget highTarget) bots

injectInitVal :: Bots -> [String] -> Bots
injectInitVal bots w =
  let val = read $ w !! 1
      botId' = BotId $ read $ w !! 5
   in passValToBot val botId' bots

passValToBot :: ElementId -> BotId -> Bots -> Bots
passValToBot val = Map.update (\b -> Just (b {botElements = val : botElements b}))

parseTarget :: String -> String -> Target
parseTarget target id' =
  if target == "bot"
  then BotTarget $ BotId (read id')
  else OutputTarget $ OutputId (read id')

runSimulationUntil :: [ElementId] -> Containers -> Bot
runSimulationUntil comboChip containers@(bots, _) =
  let bot = fromJust $ traceShowId $ findBotWith2Chips bots
   in if isBotFound comboChip bot
      then bot
      else runSimulationUntil comboChip $ execState (transferBotElements bot) containers

runFullSimulation :: Containers -> Containers
runFullSimulation containers@(bots, _) =
  let maybeBot = traceShowId $ findBotWith2Chips bots
   in case maybeBot of
        Nothing -> containers
        Just bot -> runFullSimulation $ execState (transferBotElements bot) containers

isBotFound :: [ElementId] -> Bot -> Bool
isBotFound comboChip bot = all (`elem` botElements bot) comboChip

findBotWith2Chips :: Bots -> Maybe Bot
findBotWith2Chips = fmap snd . find (\(_, v) -> length (botElements v) == 2) . Map.toList

transferBotElements :: Bot -> BotState ()
transferBotElements bot = do
  modify $ first $ clearBotElements bot
  modifyBot (minimum $ botElements bot) (low bot)
  modifyBot (maximum $ botElements bot) (high bot)
  where
    clearBotElements b = Map.adjust (const bot {botElements = []}) (botId b)
    modifyBot :: ElementId -> Target -> BotState ()
    modifyBot val (BotTarget i) = modify $ first (passValToBot val i)
    modifyBot val (OutputTarget i) = modify $ second (Map.insert i val)