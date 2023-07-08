import System.IO

takeUntil :: String -> Char -> String
takeUntil [] _ = []
takeUntil (x:xs) c
        | x == c = []
        | otherwise = x : takeUntil xs c

linesToList :: String -> [String]
linesToList [] = []
linesToList cs = take 15 cs : linesToList (drop 16 cs)

read3toIn :: Int -> String -> Int
read3toIn n cs = read ([cs !! n, cs !! (n+1), cs !! (n+2)]) :: Int

parseLine :: String -> (Int, Int, Int)
parseLine cs = (read3toIn 2 cs, read3toIn 7 cs, read3toIn 12 cs)

nthOfEach :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> ((Int, Int, Int) -> Int) -> (Int, Int, Int)
nthOfEach a b c nth = (nth a, nth b, nth c)

fst' :: (Int, Int, Int) -> Int
fst' (a,_,_) = a

snd' :: (Int, Int, Int) -> Int
snd' (_,b,_) = b

trd' :: (Int, Int, Int) -> Int
trd' (_,_,c) = c

refactorOrder :: [(Int, Int, Int)] -> [(Int, Int, Int)]
refactorOrder [] = []
refactorOrder (a:b:c:ts) = nOE fst' : nOE snd' : nOE trd' : refactorOrder ts
                           where
                                nOE = nthOfEach a b c

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a, b, c) = a + b > c && b + c > a && c + a > b

main = do
         string <- readFile "input3.txt"
         let numPossible = length $ filter isTriangle $ map parseLine $ linesToList string
         let numPossible' = length $ filter isTriangle $ refactorOrder $ map parseLine $ linesToList string
         print numPossible'
