import Common
import Data.List
import Data.Maybe
import Debug.Trace

type Component = (Char, Bool) -- Element, isGenerator
type Unit = (Int, Int) -- Generator & Chip level
type State = (Int, [Unit]) -- Elevator, Units
type Step = ((Int, Int), State) -- (#th step, position of previous step), the state
type Nth = (Int, Int) -> Int
type Move = ((Int, Int), (Nth, Nth)) -- (1st component of nth, 2nd of mth), (which to take)


sameElements :: Eq a => [a] -> [a] -> Bool
sameElements [] [] = True-- {{{
sameElements as bs
           | (head as) `elem` bs = sameElements (tail as) (bs `except` (head as))
           | otherwise = False-- }}}

isNewState :: State -> [State] -> Bool
isNewState _ [] = True-- {{{
isNewState newState (state:states) =
  (fst newState /= fst state
  || not ((snd newState) `sameElements` (snd state)))
  && isNewState newState states-- }}}

compareState :: State -> State -> Ordering
compareState (a, []) (b, []) = compare b a-- {{{
compareState a b = compare (sumLevels a) (sumLevels b)
  where sumLevels state = fst state + (sum (map (\(g, m) -> g + m) (snd state)))-- }}}

isStateValid :: State -> Bool -- no, no state has a valid claim to power
isStateValid (_, []) = False -- How can there be no units?{{{
isStateValid (elevator, units)
           | filter (inTuple elevator) units == [] = False -- Elevator can't move
           | otherwise = foldl (&&) True (map unitOK units) -- Chip is fried
             where
                  unitOK (g, m) = g == m || not (m `elem` (map fst (units `except` (g, m))))-- }}}

uniqueUnits :: State -> [Unit] -- Returns unique units on elevator's level - add as where
uniqueUnits (elevator, floors) = uniques (filter (inTuple elevator) floors)

stateFromMove :: Int -> Move -> State -> State -- Assumes movement is possible
stateFromMove dir ((a, b), (aFunct, bFunct)) (elevator, units) = -- {{{
  (elevator+dir, [if i `inTuple` (a, b)-- {{{
                then if a == b
                       then (newUnits !! i) `addTuple` (dir,dir)
                       else if i == a
                              then performFunct aFunct (newUnits !! a)
                              else performFunct bFunct (newUnits !! b)
                else (newUnits !! i)
             | i <- [0..(length newUnits -1)]])-- }}}
  where-- {{{
    addTuple = (\(c, d) (e, f) -> (c+e, d+f))
    performFunct funct unit = unit `addTuple` (funct (dir,0), funct (0,dir))
    uniqueUnits = uniques (filter (inTuple elevator) units)
    newUnits = uniqueUnits ++ units `without` uniqueUnits -- This caused so much trouble -- }}}}}}

movesFromState :: State -> [Move] -- UNITS HAVE BEEN LIMITED TO UNIQUES
movesFromState (elevator, units) = -- {{{
  [((a, b), get c)-- {{{
  |a<-[0..(length units -1)], -- All units
   b<-(-1):[a..(length units -1)], -- Possible units + nothing
   c<-[(d, e) | d <- [False,True], e <- [False,True]], -- All possibilities to send
   if b == (-1) -- If we send a nothing
     then fst c `xor` snd c -- Then can't send 2 gens/chips
     else c==c,
   if a==b then c==(False,True) else c==c, -- If from same unit then must be (0,1) sent
   adjust a c fst,
   if b < 0 then c==c else adjust b c snd]-- }}}
   where-- {{{
     get c = (if fst c then snd else fst, if snd c then snd else fst)
     adjust u c nth = 
       if fst (units !! u) == snd (units !! u) -- If unit on same floor do nothing new
         then c==c
         else if fst (units !! u) == elevator -- If then generator is on the floor send it
           then nth c == False
           else nth c == True -- Otherwise send the chip}}}}}}

statesFromState :: Int -> State -> [State] -- does not filter for previously done
statesFromState topFloor (elevator, units) = -- {{{
  reverse $ sortBy compareState $ filter isStateValid states
    where
      uniqueUnits = uniques (filter (inTuple elevator) units)
      atBottom = elevator == 0
      atTop = elevator == topFloor
      moveDirs = (if not atBottom && not atTop
                    then [(-1),1]
                    else if not atBottom
                      then [(-1)] else [1]) -- Assumes length floor > 1
      state = (elevator, units)
      states = [stateFromMove dir move state
               | dir <- moveDirs,
                 move <- movesFromState (elevator, uniqueUnits)]-- }}}

toStep :: Step -> [Step] -> [Step]
toStep ((0, m), state) steps = ((0, m), state) : []-- {{{
toStep ((n, m), state) steps = ((n, m), state) : toStep (steps !! m) steps-- }}}

-- Check 11notes.xopp
allSteps :: [Int] -> [Step] -> [Step] -- Branches (start with [0]), Steps so far
allSteps _ [] = []-- {{{
allSteps branches steps
  | length branches == 0 = steps -- All steps are known
  | otherwise = allSteps newBranches newSteps -- trace (show (currentStep, toStep currentStep steps)) $
    where-- {{{
      currentStep = steps !! head branches -- Gets current step from position of latest branch
      allStates = map (\(n, state) -> state) steps -- Gets all past states
      pastStates = map (\(n, state) -> state) (toStep currentStep steps) -- Gets all relevant past states
      genStates = statesFromState 3 (snd currentStep) -- Generates list of potential new states - assuming 4 levels
      newStates = filter (`isNewState` pastStates) genStates -- Filters to new states
      newID = ((fst . fst) currentStep + 1, head branches) -- Generates new stepID
      newSteps = steps ++ map (\state -> (newID, state)) newStates -- Adds new steps to end (therefore position conforms with ID system)
      newBranches = [(length steps)..(length newSteps - 1)] ++ tail branches -- Following ID system, positions of new branches are added -- }}}}}}

isFinished :: Int -> Step -> Bool -- All units on level n
isFinished _ (_, (_, [])) = False -- No units? --{{{
isFinished n (_, (elev, units)) =
  elev == n -- Elevator on last floor
  && sum (map (\(a,b)->a+b) units) == 2 * n * length units -- All components on level n -- }}}

howManySteps :: [Step] -> [Int]
howManySteps steps = -- {{{
  map (\steps -> (fst . fst) steps)
  $ filter (isFinished 3) steps-- }}}

showMoves :: [Move] -> String
showMoves moves = show $ map transform moves-- {{{
  where
       opts = (0,1)
       transform ((a,b),(c,d)) = ((a,b),(c opts,d opts))-- }}}

showState' :: Int -> [String] -> State -> [String]
showState' _ prevShow (_, []) = prevShow-- {{{
showState' i prevShow (elevator, (unit:units)) = showState' (i+1) newShow (elevator, units)
            where
                 addUnitID y = prevShow !! y ++ ' ':(show i)
                 newShow = [if y `inTuple` unit
                            then if fst unit == snd unit
                                 then addUnitID y ++ "G" ++ ' ':(show i) ++ "M"
                                 else if y == fst unit
                                      then addUnitID y ++ "G"
                                      else addUnitID y ++ "M"
                            else prevShow !! y | y <- [0..3]]-- }}}

showState state = "___" ++ (addLineBetween $ reverse $ [if y == fst state then '-':(floors !! y) else ' ':(floors !! y) | y <- [0..(length floors - 1)]])
                   where floors = showState' 0 (take 4 (repeat [])) state

parseLine :: String -> [Component]
parseLine [] = []-- {{{
parseLine line-- {{{
        | word 4 == "nothing" = []
        | word 3 == "contains" = parseLine beginningCut
        | function == "microchip" = (head (word 0), False) : parseLine restLine
        | otherwise = (head (word 0), True) : parseLine restLine-- }}}
          where-- {{{
               word = (`nthWord` line)
               beginningCut = dropUntilN 5 ' ' line
               hasPunctuation = last (word 1) `elem` ",."
               function = if hasPunctuation then init (word 1) else word 1
               isNextAnd = word 2 == "and"
               restLine = if isNextAnd then dropUntilN 4 ' ' line else dropUntilN 3 ' ' line-- }}}}}}

floorsToUnits' :: Int -> [[Component]] -> [Unit]
floorsToUnits' n floors-- {{{
             | length floors == length (filter (==[]) floors) = []
             | floors !! n == [] = floorsToUnits' (n+1) floors
             | otherwise = (floorGen, floorChip) : floorsToUnits' n floorsWithoutComponents
               where-- {{{
                    component = head (floors !! n)
                    restFloor = tail (floors !! n)
                    restFloors = floors `except` (floors !! n)
                    rest = (take n floors) ++ (restFloor:[]) ++ (drop (n+1) floors) 
                    otherComponent = (fst component, not (snd component))
                    floorOtherComponent = fromJust $ elemIndex (otherComponent:[]) $ map (filter (== otherComponent)) rest
                    floorGen = if snd component then n else floorOtherComponent
                    floorChip = if snd otherComponent then n else floorOtherComponent
                    floorsWithoutComponents = [if y == floorOtherComponent then (rest !! y) `except` otherComponent else (rest !! y)
                                              | y <- [0..(length floors - 1)]]-- }}}}}}

floorsToUnits floors = floorsToUnits' 0 floors

main = do
         lines <- readFile "input11_2.txt"
         let units = floorsToUnits $ map parseLine $ linesToList lines
         -- let units = [(0,0)]
         let steps = allSteps [0] [((0 ,0), (0, units))]
         let nSteps = howManySteps steps
         let fstSolution = head $ filter (isFinished 3) steps
         let pathToSolution = reverse $ toStep fstSolution steps
         let statesOfPath = map snd pathToSolution
         print nSteps
         -- putStrLn $ addLineBetween $ map showState statesOfPath
