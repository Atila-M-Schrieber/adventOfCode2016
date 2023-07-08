import Common

type MarkerData = (Int, String)

parseLine :: String -> [MarkerData]
parseLine [] = []
parseLine compressed
        | '(' == head compressed = (times, mdata) : parseLine (drop nchars (dropUntil ')' compressed))
        | otherwise = (1, takeUntil '(' compressed) : parseLine (dropTo '(' compressed)
          where
               nchars = read (takeUntil 'x' (tail compressed)) :: Int
               times = read (takeUntil ')' (dropUntil 'x' compressed)) :: Int
               mdata = take nchars (dropUntil ')' compressed)

decompress :: [MarkerData] -> String
decompress [] = []
decompress ((times, mdata):rest)
         | times == 1 = mdata ++ decompress rest
         | otherwise = mdata ++ decompress ((times - 1, mdata):rest)

recursiveDecompress :: String -> String
recursiveDecompress input
                  | '(' `elem` input = recursiveDecompress ((decompress . parseLine) input)
                  | otherwise = input

finalLen :: [MarkerData] -> Int
finalLen [] = 0
finalLen ((times, mdata):rest)
       | '(' `elem` mdata = times * (finalLen . parseLine) mdata + finalLen rest
       | otherwise = times * (length mdata) + finalLen rest

main = do
         compressed <- readFile "input9.txt"
         let decompressed = map (decompress . parseLine)  $ linesToList compressed 
         let len = sum $ map length decompressed
         let len' = sum $ map finalLen $ map parseLine $ linesToList compressed
         print len'
