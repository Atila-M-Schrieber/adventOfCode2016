module Common where 
import Data.Maybe
import Data.List

xor :: Bool -> Bool -> Bool
xor a b = (a||b) && (not (a&&b))

inTuple :: Eq a => a -> (a, a) -> Bool
inTuple a (b, c) = a == b || a == c

hasN :: Eq a => Int -> a -> [a] -> Bool
hasN n c cs
  | length cs < n = False
  | otherwise = take n (repeat c) == take n cs || if c `elem` cs then hasN n c (dropTo c (tail cs)) else False

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ [] = []
takeUntil c (x:xs)
        | x == c = []
        | otherwise = x : takeUntil c xs

dropUntil :: Eq a => a -> [a] -> [a]
dropUntil c as = drop (length (takeUntil c as) + 1) as

dropUntilN :: Eq a => Int -> a -> [a] -> [a]
dropUntilN 0 _ as = as
dropUntilN n c as = dropUntilN (n-1) c $ dropUntil c as

dropTo :: Eq a => a -> [a] -> [a]
dropTo c as = drop (length (takeUntil c as)) as

between :: Eq a => a -> a -> [a] -> [a]
between a b cs = takeUntil b (dropUntil a cs)

except :: Eq a => [a] -> a -> [a]
except [] _ = []
except as a
     | a `elem` as = take pos as ++ drop (pos+1) as
     | otherwise = as
       where 
            pos = fromJust $ elemIndex a as

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques [a] = [a]
uniques (a:as)
      | a `elem` as = a : (uniques (as `except` a))
      | otherwise = a : (uniques as)

without :: Eq a => [a] -> [a] -> [a]
without as [] = as
without [] _ = []
without as (b:bs) = without (if b `elem` as then as `except` b else as) bs


addLineBetween' :: String -> String -> String
addLineBetween' a b = a ++ ('\n':[]) ++ b

addLineBetween as = foldl addLineBetween' [] as

nthWord :: Int -> String -> String
nthWord 0 string = takeUntil ' ' string
nthWord n string = nthWord (n-1) $ dropUntil ' ' string

toInt :: String -> Int
toInt int = read int :: Int

linesToList :: String -> [String]
linesToList [] = []
linesToList cs = line : linesToList (drop (1 + length line) cs)
                 where
                      line = takeUntil '\n' cs
