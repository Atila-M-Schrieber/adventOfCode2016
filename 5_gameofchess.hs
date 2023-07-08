import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)

startsWithN :: Eq a => Int -> a -> [a] -> Bool
startsWithN n c cs = take n (repeat c) == take n cs

genFromHash :: Int -> Int -> String -> String
genFromHash n i cs
          | startsWithN n '0' hash = hash !! n : genFromHash n (i+1) cs
          | otherwise = genFromHash n (i+1) cs
            where
                 hash = show $ md5 $ pack $ cs ++ show i

genFromHash' :: Int -> Int -> String -> [(Int, Char)]
genFromHash' n i cs
          | startsWithN n '0' hash && hash !! n `elem` ['0'..'7']= (read (hash !! n : []) :: Int, hash !! (n+1)) : genFromHash' n (i+1) cs
          | otherwise = genFromHash' n (i+1) cs
            where
                 hash = show $ md5 $ pack $ cs ++ show i

fromGen :: [(Int, Char)] -> String -> String
fromGen ((pos,char):rest) ss
          | length (filter (== ' ') ss) == 0 = ss
          | ss !! pos == ' ' = fromGen rest (take pos ss ++ char:[] ++ drop (pos+1) ss)
          | otherwise = fromGen rest ss

passFromGen :: [(Int, Char)] -> String
passFromGen gen = fromGen gen $ take 8 (repeat ' ')

main = do
         doorID <- readFile "input5.txt"
         let password = take 8 $ genFromHash 5 0 $ init doorID
         let password' = passFromGen $ genFromHash' 5 0 $ init doorID
         print password'
