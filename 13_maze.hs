import Common

type Coordinate = (Int, Int) -- x, y
type Maze = [[Int]] -- isOpen @ x,y but as ints


intToBinary :: Int -> [Int] -- num, binary num as 'bits'
intToBinary 0 = []
intToBinary n = intToBinary (n `div` 2) ++ n `mod` 2 : []

isOpen :: Coordinate -> Int -> Bool -- Coordinate & fave number of designer
isOpen (x, y) n = x >= 0 && y >= 0 && ((`mod` 2) . sum . intToBinary) (x*x + 3*x + 2*x*y + y + y*y + n) == 0

isWall coord n = not $ isOpen coord n

genMaze :: Coordinate -> Int -> Maze -- (0,0) to limit with faveNum
genMaze (xmax, ymax) n = [[if isOpen (x, y) n then (-1) else (-2) | x <- [0..xmax]] | y <- [0..ymax]]

mazeToLines :: Maze -> Int -> [String] -- Maze with faveNum
mazeToLines maze n = -- {{{
  ("  " ++ [(last . show) i | i <- [0..((length . head) maze -1)]] ++ "\n") :
  [[if x == (-2) then (last . show) y else if x == (-1) then ' ' else if x == ((length . head) maze) then '\n' else
      if (maze !! y) !! x  == (-1) then '.' else '#'
      | x <- [(-2)..((length . head) maze)] ] | y <- [0..(length maze -1)] ]-- }}}

showMaze :: Maze -> Int -> String
showMaze maze n = foldl (++) [] $ mazeToLines maze n

genSteps :: [(Coordinate, Int)] -> Int -> [(Coordinate, Int)] -- from starting coord & fave num, take each neighbor, see steps -- infinite list to all points
genSteps [] _ = []-- {{{
genSteps steps n = steps ++ newSteps ++ genSteps (steps ++ newSteps) n
    where
         genNewStep step dx dy = (((fst . fst) step + dx, (snd . fst) step + dy), snd step + 1) :: (Coordinate, Int)
         newSteps = [ newStep-- {{{
                      | dx <- [(-1)..1], dy <- [(-1)..1], step <- steps, dx + dy `elem` [(-1),1],
                        let newStep = genNewStep step dx dy,
                        isOpen (fst newStep) n, not (fst newStep `elem` map fst steps)]-- }}}}}}

genStepsMax :: [(Coordinate, Int)] -> Int -> Int -> [(Coordinate, Int)] -- start, max steps, fave num
genStepsMax [] _ _ = []-- {{{
genStepsMax steps max n
  | newSteps == [] = steps
  | otherwise = uniques (steps ++ newSteps ++ genStepsMax (steps ++ newSteps) max n)
    where
         genNewStep step dx dy = (((fst . fst) step + dx, (snd . fst) step + dy), snd step + 1) :: (Coordinate, Int)
         newSteps = [ newStep-- {{{
                      | dx <- [(-1)..1], dy <- [(-1)..1], step <- steps, dx + dy `elem` [(-1),1],
                        let newStep = genNewStep step dx dy,
                        isOpen (fst newStep) n, not (fst newStep `elem` map fst steps), snd newStep <= max]-- }}}}}}

stepsFromTo :: Coordinate -> Coordinate -> Int -> Int -- From, To, FaveNum -> number of steps
stepsFromTo start end n = (snd . head . filter (\step -> fst step == end)) $ genSteps [(start, 0)] n

main1 = do-- {{{
         putStr "Input X: "
         xs <- getLine
         let x = read xs :: Int
         putStr "Input Y: "
         ys <- getLine
         let y = read ys :: Int
         putStr "Input the designer's favorite number: "
         ns <- getLine
         let n = read ns :: Int
         print $ stepsFromTo (1,1) (x,y) n-- }}}

main = do
         print $ length $ genStepsMax [((1, 1), 0)] 50 1362
