import Data.List

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

timesIn :: String -> [(Char, Int)] 
timesIn [] = []
timesIn (c:cs) = (c, 1 + length (filter (== c) cs)) : timesIn (filter (/= c) cs)

compare' :: Ord a => (a, Int) -> (a, Int) -> Ordering
compare' (a, n) (b, m)
       | n /= m = compare n m
       | otherwise = compare b a

sort' :: Ord a => [(a, Int)] -> [(a, Int)]
sort' ts = sortBy (\a b -> compare' a b) ts

retrieveMessage :: [String] -> String
retrieveMessage [[]] = []
retrieveMessage repeats
              | length (head repeats) == 0 = []
              | otherwise = char : retrieveMessage (map tail repeats)
                where
                     char = fst $ head $ sort' $ timesIn $ map head repeats

main = do
         noisy <- readFile "input6.txt"
         let message = retrieveMessage $ linesToList noisy
         print message
