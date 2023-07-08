import Common
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)

genKeys :: String -> [(String, Int)] -- salt, index
genKeys salt = keys
  where
       genHash index = show $ md5 $ pack $ salt ++ show index
       hashNTimes 0 string = string
       hashNTimes n string = hashNTimes (n-1) $ show $ md5 $ pack $ string
       genLongHash index = hashNTimes 2017 $ salt ++ show index
       has3of [] = 'N' -- No 
       has3of hash = if hasN 3 (head hash) hash then head hash else has3of $ tail hash
       keys = [(hash, index) | index <- [22551..],
                let hash = genLongHash index, let c = has3of hash, c /= 'N',
                let fives = (head . filter (\hash' -> hasN 5 c (fst hash'))) [(genLongHash i, i) | i <- [(index + 1)..(index + 1001)] ],
                (length . filter (\hash' -> hasN 5 c hash')) [genLongHash i | i <- [(index + 1)..(index + 1001)] ] > 0 ]

main = do
         salt <- getLine
         let keys = take 65 $ genKeys salt
         print $ (snd . last) keys
