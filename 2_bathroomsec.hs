
parse :: String -> [(Int, Int)]
parse [] = []
parse (c:cs)
    | c == 'U' = (0, 1) : parse cs
    | c == 'D' = (0, (-1)) : parse cs
    | c == 'L' = ((-1), 0) : parse cs
    | c == 'R' = (1, 0) : parse cs
    | otherwise= (0, 0) : parse cs

parseLines :: [String] -> [[(Int, Int)]]
parseLines [] = []
parseLines (s:ss) = parse s : parseLines ss

keymap :: (Int, Int) -> Int
keymap (a, b)
     | b == 1 && a == (-1) = 1
     | b == 1 && a == 0 = 2
     | b == 1 && a == 1 = 3
     | b == 0 && a == (-1) = 4
     | b == 0 && a == 0 = 5
     | b == 0 && a == 1 = 6
     | b == (-1) && a == (-1) = 7
     | b == (-1) && a == 0 = 8
     | b == (-1) && a == 1 = 9
     | otherwise = 0

keymap' :: (Int, Int) -> Char
keymap' (a, b)
     | b == 2 && a == 0 = '1'
     | b == 1 && a == (-1) = '2'
     | b == 1 && a == 0 = '3'
     | b == 1 && a == 1 = '4'
     | b == 0 && a == (-2) = '5'
     | b == 0 && a == (-1) = '6'
     | b == 0 && a == 0 = '7'
     | b == 0 && a == 1 = '8'
     | b == 0 && a == 2 = '9'
     | b == (-1) && a == (-1) = 'A'
     | b == (-1) && a == 0 = 'B'
     | b == (-1) && a == 1 = 'C'
     | b == (-2) && a == 0 = 'D'
     | otherwise = '0'

foldInt :: [Int] -> Int
foldInt [] = 0
foldInt [x] = x
foldInt (x:xs) = (x * 10^(length xs)) + foldInt xs

vectorAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vectorAdd (a, b) (c, d) = (a + c, b + d)

posConstrain :: (Int, Int) -> (Int, Int)
posConstrain (a, b) = (signum a, signum b)

vecConAdd' :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecConAdd' a b = posConstrain $ vectorAdd a b

taxicabDistance :: (Int, Int) -> Int
taxicabDistance (i, j) = abs i + abs j

vecConAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecConAdd a b
        | taxicabDistance sum > 2 = a
        | otherwise = sum
          where
               sum = vectorAdd a b

keypos :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
keypos pos moves = foldl vecConAdd pos moves

keyPositions :: (Int, Int) -> [[(Int, Int)]] -> [(Int, Int)]
keyPositions _ [] = []
keyPositions p (x:xs) = pos : keyPositions pos xs
                        where 
                             pos = keypos p x

-- getCode :: String -> Int
-- getCode cs = foldInt $ map (keymap . keypos) (lines cs)

getLines :: IO [String]
getLines = do x <- getLine
              if x == ""
                then return []
                else do xs <- getLines
                        return (x:xs)

main = do
         instructions <- getLines
         let code = map keymap' $ keyPositions ((-2),0) $ parseLines instructions
         print code
