import Data.List
import Data.Maybe
import Debug.Trace

takeUntil :: String -> Char -> String
takeUntil [] _ = []
takeUntil (x:xs) c
        | x == c = []
        | otherwise = x : takeUntil xs c

parseDirections :: String -> [(Char, Int)]
parseDirections [] = []
parseDirections dirs = (head dirs, read (tail (take n dirs)) :: Int) : parseDirections (drop (n+2) dirs)
                      where
                           n = length $ takeUntil dirs ','

rotateDir :: (Int, Int) -> Char -> (Int, Int) -- R and L would be opposite if we were rotating the vector, but we are rotating the 'whole plane'
rotateDir (i, j) 'R' = (j, (-i))
rotateDir (i, j) 'L' = ((-j), i)
rotateDir _ _ = (0, 0)

route :: [(Char, Int)] -> (Int, Int) -> (Int, Int)
route [] _ = (0, 0)
route ((dir,len):dirs) vec = (x, y)
                            where
                                 prev = route dirs (rotateDir vec dir)
                                 direction = rotateDir vec dir
                                 x = fst prev + (len * fst direction) 
                                 y = snd prev + (len * snd direction)

routeFollow dirs = route dirs (0, 1)

routeTrack :: [(Char, Int)] -> [(Int, Int)]
routeTrack [] = [(0, 0)]
routeTrack dirs = routeTrack (init dirs) ++ routeFollow dirs : []

rangeBetween :: Int -> Int -> [Int]
rangeBetween a b
           | a < b  = [a..b]
           | otherwise = [b..a]

elemOfRanges :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
elemOfRanges a b c d e f = a `elem` (rangeBetween b c) && d `elem` (rangeBetween e f)

lineIntersectBool :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
lineIntersectBool ((a, b), (c, d)) ((e, f), (g, h))
                | e == g = elemOfRanges e a c b f h
                | a == c = elemOfRanges a e g f b d
                | otherwise = False

-- CHECK WITH BOOL FIRST
lineIntersectPos :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> (Int, Int)
lineIntersectPos ((a, b), (c, d)) ((e, f), (g, h))
               | e == g && elemOfRanges e a c b f h = (e, b)
               | a == c && elemOfRanges a e g f b d = (a, f)

lineInRoute :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
lineInRoute line ps
          | length ps < 2 = []
          | lineIntersectBool line nextLine = lineIntersectPos line nextLine : lineInRoute line (tail ps)
          | otherwise = lineInRoute line (tail ps)
            where
                 nextLine = (head ps, ps !! 1)

-- Pass in reversed track for results in 'correct order'
routeIntersect :: [(Int, Int)] -> [(Int, Int)]
routeIntersect [] = []
routeIntersect [point] = []
routeIntersect (p:q:rest) = lineInRoute (p, q) rest ++ routeIntersect rest

taxicabDistance :: (Int, Int) -> Int
taxicabDistance (i, j) = abs i + abs j

main = do
    let directions = "R4, R1, L2, R1, L1, L1, R1, L5, R1, R5, L2, R3, L3, L4, R4, R4, R3, L5, L1, R5, R3, L4, R1, R5, L1, R3, L2, R3, R1, L4, L1, R1, L1, L5, R1, L2, R2, L3, L5, R1, R5, L1, R188, L3, R2, R52, R5, L3, R79, L1, R5, R186, R2, R1, L3, L5, L2, R2, R4, R5, R5, L5, L4, R5, R3, L4, R4, L4, L4, R5, L4, L3, L1, L4, R1, R2, L5, R3, L4, R3, L3, L5, R1, R1, L3, R2, R1, R2, R2, L4, R5, R1, R3, R2, L2, L2, L1, R2, L1, L3, R5, R1, R4, R5, R2, R2, R4, R4, R1, L3, R4, L2, R2, R1, R3, L5, R5, R2, R5, L1, R2, R4, L1, R5, L3, L3, R1, L4, R2, L2, R1, L1, R4, R3, L2, L3, R3, L2, R1, L4, R5, L1, R5, L2, L1, L5, L2, L5, L2, L4, L2, R3"
    let distance = (taxicabDistance . routeFollow . parseDirections) directions
    let distToFstIntersect = (taxicabDistance . head . routeIntersect . routeTrack . parseDirections) directions
    print distance
    print distToFstIntersect
