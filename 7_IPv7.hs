import Data.List
import Data.Tuple
import Control.Applicative

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ [] = []
takeUntil c (x:xs)
        | x == c = []
        | otherwise = x : takeUntil c xs

linesToList :: String -> [String]
linesToList [] = []
linesToList cs = line : linesToList (drop (1 + length line) cs)
                 where
                      line = takeUntil '\n' cs

parseLine :: String -> [(Bool, String)]
parseLine [] = []
parseLine cs
        | isBeforeHS = (False, takeUntil '[' cs) : parseLine (drop (length  (takeUntil '[' cs) + 1) cs)
        | isInHS = (True, takeUntil ']' cs) : parseLine (drop (length (takeUntil ']' cs) + 1) cs)
        | otherwise = (False, cs) : []
          where
               isBeforeHS = '[' `elem` cs && (length (filter ( == '[') cs)) == (length (filter ( == ']') cs))
               isInHS = (length (filter ( == '[') cs)) == (length (filter ( == ']') cs)) - 1

hasABBA :: String -> Bool
hasABBA cs
        | length cs < 4 = False
        | isABBA = True
        | otherwise = hasABBA $ tail cs
          where
               isABBA = head cs /= cs !! 1 && head cs == cs !! 3 && cs !! 1 == cs !! 2

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

supportsTLS :: [(Bool, String)] -> Bool
supportsTLS [] = False
supportsTLS [(isInHS, part)] = (isInHS `xor` hasABBA part)
supportsTLS ((isInHS, part):rest)
          | isInHS && hasABBA part = False
          | not isInHS && hasABBA part = supportsTLS $ filter fst rest
          | otherwise = supportsTLS rest

hasABA :: (Char, Char) -> String -> Bool
hasABA (a, b) cs
     | length cs < 3 = False
     | a == b = False
     | head cs == a && cs !! 1 == b && cs !! 2 == a = True
     | otherwise = hasABA (a, b) (tail cs)

findABA :: String -> [(Char, Char)]
findABA cs = filter (`hasABA` cs) allLetterPairs
        where
             allLetterPairs = liftA2 (,) ['a'..'z'] ['a'..'z']

uniteList :: [[a]] -> [a]
uniteList [] = []
uniteList [l] = l
uniteList (l:ls) = l ++ uniteList ls

containCommon :: Eq a => [a] -> [a] -> Bool
containCommon [] [] = False
containCommon _ [] = False
containCommon [] _ = False
containCommon (a:as) bs = a `elem` bs || containCommon as bs

supportsSSL :: [(Bool, String)] -> Bool
supportsSSL [] = False
supportsSSL [_] = False
supportsSSL sequences = containCommon allABAinSS $ map swap allABAinHS
            where
                 allABAinSS = uniteList $ map findABA $ map snd $ filter (not . fst) sequences
                 allABAinHS = uniteList $ map findABA $ map snd $ filter (fst) sequences

main = do
         ips <- readFile "input7.txt"
         let numIPs = length $ filter supportsTLS $ map parseLine $ linesToList $ ips
         let numIPs' = length $ filter supportsSSL $ map parseLine $ linesToList $ ips
         print numIPs'
