import Common

type Screen = [[Bool]]
type Instruction = ((Bool, Bool), (Int, Int)) -- ((isRect, isColumn), vector {x by y rect / column/row, rotation amount})

screenToVis :: Screen -> [String]
screenToVis screen = map (map (\x->if x==True then '#' else ' ')) screen

genScreen :: (Int, Int) -> Screen
genScreen (x, y) = take y $ repeat $ take x $ repeat False

parseLine :: String -> Instruction
parseLine input
        | takeUntil ' ' input == "rect" = ((True, False), rectVals)
        | takeUntil ' ' input == "rotate" = ((False, isColumn), rotateVals)
        | otherwise = ((False, False), (0, 0))
          where
               rectVals = (read (between ' ' 'x' input) :: Int, read (dropUntil 'x' input) :: Int)
               isColumn = "column" == between ' ' ' ' input
               lastVal = (dropUntil ' ' . dropUntil ' ' . dropUntil ' ' . dropUntil ' ') input
               rotateVals = (read (between '=' ' ' input) :: Int, read lastVal :: Int)

operation :: Instruction -> Screen -> Screen
operation ((isRect, isColumn), (xcolrow, yamount)) screen
        | isRow = [if y == xcolrow 
                   then [x |
                           i <- xs,
                           let x = currentRow !! ((i - yamount) `mod` (length currentRow))]
                   else screen !! y
                   | y <- ys]
        | isCol = [[if i == xcolrow
                    then (screen !! ((y - yamount) `mod` (length screen))) !! i
                    else (screen !! y) !! i
                    | i <- xs]
                      | y <- ys]
        | otherwise = [if y < yamount
                       then [if x < xcolrow
                             then True
                             else (screen !! y) !! x
                             | x <- xs]
                       else screen !! y
                       | y <- ys]
          where
               isRow = not isRect && not isColumn
               ys = [0..(length screen - 1)]
               xs = [0..((length . head) screen - 1)]
               isCol = not isRect && isColumn
               currentRow = screen !! xcolrow

main = do
         instructions <- readFile "input8.txt"
         let screen = foldl (\scr inst -> operation inst scr) (genScreen (50, 6)) $ map parseLine $ linesToList instructions
         let numOn = sum $ map (length . filter (==True)) screen
         putStrLn $ addLineBetween $ screenToVis screen
