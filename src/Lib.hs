module Lib
    ( runUniversalTuringMachine
    ) where

import Debug.Trace (trace)

-- Configuration for multiplying
multiplierConfiguration :: String
multiplierConfiguration = "010100100010011010001000000000010001011001010010100110010001000100010011000101000010001001100010001000000001000101100001010000101001100001000100000100010011000001010000010100110000010001000000101011000000101000000101011000000100010000000100010110000000101000000010101100000001000100010100110000000010100000000101011000000001000100000000010001011000000000101000000000101011000000000100010101001100000000001010000000000101011000000000010001000000000001000100"

-- 2 x 4
input :: String
input = "00 0000"
-- End Configuration for multiplying

empty :: Char
empty = ' '

separator :: Char
separator = '1'

runUniversalTuringMachine :: IO ()
runUniversalTuringMachine = do
   let configuration = parseConfiguration multiplierConfiguration
   let initialState = MachineState { tapeLeft = [empty], tapeRight = input, currentState = 1 }
   let finalState = show (applyTransitions (nextTransition initialState configuration) configuration initialState)
   putStrLn ("Resulting state: " ++ finalState)


data Transition = Transition { startState :: Int, endState :: Int, tapeIn :: Char, tapeOut :: Char, next :: HeadDirection } deriving (Show)
data HeadDirection = MoveLeft | MoveRight deriving (Enum, Show, Eq)
data MachineState = MachineState { tapeLeft :: [Char], tapeRight :: [Char], currentState :: Int } deriving (Show)

applyTransitions :: Maybe Transition -> [Transition] -> MachineState -> MachineState
applyTransitions _ _ state | trace ("current state: " ++ show state) False = undefined
applyTransitions Nothing _ state = state
applyTransitions _ [] state = state
applyTransitions (Just transition) transitions state =
   let
      nextState = updateState (moveHead (writeCharacter state (tapeOut transition)) (next transition)) (endState transition)
   in
      applyTransitions (nextTransition nextState transitions) transitions nextState

updateState :: MachineState -> Int -> MachineState
updateState state newState = MachineState { tapeLeft = tapeLeft state, tapeRight = tapeRight state, currentState = newState }

writeCharacter :: MachineState -> Char -> MachineState
writeCharacter state character =
  let
    newTapeRight = character : tail (tapeRight state)
  in
     MachineState { tapeLeft = tapeLeft state, tapeRight = newTapeRight, currentState = currentState state }

moveHead :: MachineState -> HeadDirection -> MachineState
moveHead state headDirection =
  let
    newTapeLeft = if headDirection == MoveLeft then reduceOneRight $ tapeLeft state else addRight (tapeLeft state) (head $ tapeRight state)
    newTapeRight = if headDirection == MoveLeft then last (tapeLeft state) : tapeRight state else reduceOneLeft $ tapeRight state
  in
  MachineState { tapeLeft = newTapeLeft, tapeRight = newTapeRight, currentState = currentState state}

nextTransition :: MachineState -> [Transition] -> Maybe Transition
nextTransition  _ [] = Nothing
nextTransition state transitions = findFirst $ filter (\transition -> (&&) ((==) (startState transition) (currentState state))  ((==) (tapeIn transition) (head $ tapeRight state))) transitions

findFirst :: [a] -> Maybe a
findFirst [] = Nothing
findFirst (x:_) = Just x


reduceOneRight :: [Char] -> [Char]
reduceOneRight [] = [empty]
reduceOneRight [_] = [empty]
reduceOneRight x = take (length x - 1) x

addRight :: [Char] -> Char -> [Char]
addRight xs character = foldr (:) [character] xs

reduceOneLeft :: [Char] -> [Char]
reduceOneLeft [] = [empty]
reduceOneLeft [_] = [empty]
reduceOneLeft (_:x) = x

parseConfiguration :: [Char] -> [Transition]
parseConfiguration rows = map (mapToTransitions . splitRow) (splitIntoRows rows)

splitIntoRows :: [Char] -> [[Char]]
splitIntoRows = foldr (\character result -> 
  if character == separator && containsFirst result character 
  then [] : removeFirst result 
  else appendChar character result) 
  []

appendChar :: Char -> [[Char]] -> [[Char]]
appendChar character [] = [[character]]
appendChar character [[]] = [[character]]
appendChar character ([] : a) = [character] : a
appendChar character (a : b) = (character : a) : b


removeFirst :: [[Char]] -> [[Char]]
removeFirst [] = []
removeFirst [[]] = [[]]
removeFirst ([] : a) = [] : a
removeFirst (a:b) =  tail a : b

containsFirst :: [[Char]] -> Char -> Bool
containsFirst [] _ = False
containsFirst [[]] _ = False
containsFirst ([]:_) _ = False
containsFirst list character = (==) (head $ head list) character


splitRow :: [Char] -> [[Char]]
splitRow = foldr (\character result -> if character == separator then []:result else appendChar character result) []

mapToTransitions :: [[Char]] -> Transition
mapToTransitions [start, tapeI, end, tapeO, headD] =
  Transition { startState = length start, endState = length end, tapeIn = toTapeValue tapeI, tapeOut = toTapeValue tapeO, next = toHeadDirection headD }
mapToTransitions x = errorWithoutStackTrace ("Illegal encoded row:" ++ show x)

toTapeValue :: [Char] -> Char
toTapeValue xs
  | length xs == 3 = empty
  | otherwise =  head $ show ((-) (length xs) 1)

toHeadDirection :: [Char] -> HeadDirection
toHeadDirection x
  | length x == 1 = MoveLeft
  | otherwise = MoveRight