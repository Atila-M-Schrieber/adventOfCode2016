import Data.Maybe
import Data.List

import Common

type Robot = (Int, [Int]) -- ID, (values) 
-- can be output too with -(ID+1) as fst!!
type Instruction = ((Int, Bool), (Int, Int)) -- instructions ((which, isGets), (low/val, high/0)) - output is negative

robotHas2 :: Robot -> Bool
robotHas2 bot = 2 == length (snd bot)

robotFind :: Int -> [Robot] -> Robot
robotFind n [] = (n, [])
robotFind n (bot:rest)
        | fst bot == n = bot
        | otherwise = robotFind n rest

robotToFront :: Int -> [Robot] -> [Robot]
robotToFront n bots
           | bot `elem` bots = bot : bots `except` bot
           | otherwise = bot : bots
             where
                  bot = robotFind n bots

robotAddValue :: Int -> Int -> [Robot] -> [Robot] -- always low to high order for robots
robotAddValue id v bots
            | snd bot == [] = (fst bot, [v]) : tail newBots
            | fst bot < 0 || v < (head . snd) bot = (fst bot, v:(snd bot)) : tail newBots
            | otherwise = (fst bot, (snd bot) ++ v:[]) : tail newBots
              where
                   newBots = robotToFront id bots
                   bot = head newBots

robotGive :: Int -> Int -> Int -> [Robot] -> [Robot] -- only call if length (snd bot) == 2
robotGive id low high bots = (fst bot, []) : botLow : botHigh : drop 3 newBots
          where
               newBots = robotToFront id $ robotToFront low $ robotToFront high bots
               bot = head newBots
               botLow = head $ robotAddValue (fst (newBots !! 1)) ((head . snd) bot) bots
               botHigh = head $ robotAddValue (fst (newBots !! 2)) (((!! 1) . snd) bot) bots

-- Int should start at 0, second Int is "n times" for steps tracking (neg for until done)
performInss :: Int -> Int -> [Instruction] -> [Robot] -> [Robot]
performInss _ _ [] bots = bots
performInss start times inss bots
                  | times == 0 = bots
                  | isGets = performInss 0 (times-1) restIns $ robotAddValue id lowval bots
                  | canGive = performInss 0 (times-1) restIns $ robotGive id lowval high0 bots
                  | otherwise = performInss (start+1) times inss bots
                    where
                         ins = inss !! start
                         restIns = inss `except` ins
                         ((id, isGets), (lowval, high0)) = ins
                         canGive = not isGets && robotHas2 (robotFind id bots)

trackInstructions :: Int -> [Instruction] -> [Robot] -> [[Robot]]
trackInstructions 0 _ bots = bots : []
trackInstructions acc inss bots = performInss 0 acc inss bots : trackInstructions (acc - 1) inss bots

trackInss :: [Instruction] -> [[Robot]]
trackInss inss = trackInstructions (length inss) inss []

findBotWith :: [Int] -> [Robot] -> Robot
findBotWith _ [] = (0, [(-1)])
findBotWith with (bot:bots)
          | snd bot == with = bot
          | otherwise = findBotWith with bots

parseLine :: String -> Instruction
parseLine line
        | word 0 == "value" = ((int 5, True), (int 1, 0))
        | otherwise = ((int 1, False), (idLow, idHigh))
          where
               word = (`nthWord` line)
               int = (toInt . word)
               idLow = if word 5 == "output" then -(int 6 + 1) else int 6
               idHigh = if word 10 == "output" then -(int 11 + 1) else int 11

main = do
         ins <- readFile "input10.txt"
         let instructions = map parseLine $ linesToList ins
         let done = performInss 0 (-1) instructions []
         let steps = foldl (++) [] $ trackInss instructions
         let thebot = findBotWith [17, 61] steps
         let chipInOutput outID = (head . snd) $ robotFind ((-1) - outID) done
         let multiOut = chipInOutput 0 * chipInOutput 1 * chipInOutput 2
         -- print thebot
         print multiOut
         -- writeFile "output10.txt" $ show steps
