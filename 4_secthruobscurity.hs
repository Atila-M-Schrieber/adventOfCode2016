import Data.List
import Data.Function
import System.IO

-- https://stackoverflow.com/a/17253092 - black magic
lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

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

parseLine :: String -> ((String, Int), String)
parseLine line = ((chars, secID), checksum)
                 where
                      chars = take (length line - 11) line
                      secID = read (take 3 $ lastN 10 line) :: Int
                      checksum = init $ lastN 6 line

timesIn :: String -> [(Char, Int)] 
timesIn [] = []
timesIn (c:cs) = (c, 1 + length (filter (== c) cs)) : timesIn (filter (/= c) cs)

compare' :: Ord a => (a, Int) -> (a, Int) -> Ordering
compare' (a, n) (b, m)
       | n /= m = compare n m
       | otherwise = compare b a

sort' :: Ord a => [(a, Int)] -> [(a, Int)]
sort' ts = sortBy (\a b -> compare' a b) ts

mkChecksum :: String -> String
mkChecksum cs
         | length cs < 5 = "ERROR"
         | otherwise = take 5 $ map (\(c,_) -> c) $ (reverse . sort' . timesIn) cs

isRealRoom :: ((String, Int), String) -> Bool
isRealRoom ((chars, _), checksum) = mkChecksum (filter (`elem` ['a'..'z']) chars) == checksum

indexN :: Eq a => Int -> a -> [a] -> Int
indexN n _ [] = n
indexN n c cs
     | c == head cs = n
     | otherwise = indexN (n+1) c $ tail cs

indexAlphabet :: Char -> Int
indexAlphabet c = indexN 0 c ['a'..'z']

shiftChar :: Int -> Char -> Char
shiftChar n c = ['a'..'z'] !! ((indexAlphabet c - n) `mod` 26)

decryptString :: Int -> String -> String
decryptString _ [] = []
decryptString n (c:cs)
            | c `elem` ['a'..'z'] = shiftChar (-n) c : decryptString n cs
            | c == '-' = ' ' : decryptString n cs
            | otherwise = '?' : decryptString n cs

roomName :: ((String, Int), String) -> String
roomName ((chars, n), _) = decryptString n chars

showNames :: [((String, Int), String)] -> String
showNames [] = []
showNames (r:rs) = show ((snd . fst) r) ++ ": " ++ roomName r ++ "\n" ++ showNames rs

main = do
         string <- readFile "input4.txt"
         let realRooms = filter isRealRoom $ map parseLine $ linesToList string
         let sumId = sum $ map (snd . fst) $ realRooms
         let names = showNames realRooms
         -- print $ take 5 $ linesToList string
         print sumId
         -- print $ take 5 $ linesToList names
         writeFile "output4.txt" names
