import Common

type Instructions = (Int, [String]) -- Current pos
type Registers = [Int] -- each register


argsToRegs :: String -> [(Bool, Int)] -- List of # of register, negative if number
argsToRegs [] = [-- {{{]
argsToRegs args
  | length args == 1 = (True, nOf (head args)) : []
  | firstIsReg && secondIsReg = (True, nOf (head args)) : (True, nOf (last args)) : []
  | firstIsReg && not secondIsReg = (True, nOf (head args)) : (False, (read (dropUntil ' ' args) :: Int)) : []
  | not firstIsReg && secondIsReg = (False, (read (takeUntil ' ' args) :: Int)) : (True, nOf (last args))  : []
  | otherwise = (False, (read (takeUntil ' ' args) :: Int)) : (False, (read (dropUntil ' ' args) :: Int)) : []
    where-- {{{
         firstIsReg = head args `elem` ['a'..'d']
         secondIsReg = last args `elem` ['a'..'d']
         nOf char = case char of {-- {{{
           'a' -> 0;
           'b' -> 1;
           'c' -> 2;
           'd' -> 3; }-- }}}}}}}}}

performInstructions :: Instructions -> Registers -> (Int, Registers)
performInstructions (_, []) regs = (0, regs) -- Done on empty instructions{{{
performInstructions (pos, instructions) registers
  | pos >= length instructions = (pos, registers) -- Done on position is past last instruction
  | otherwise = performInstructions (newPos, instructions) newRegs
    where-- {{{
         inst = nthWord 0 (instructions !! pos)
         args = dropUntil ' ' (instructions !! pos)
         isInc = inst == "inc"
         isDec = inst == "dec"
         isCpy = inst == "cpy"
         isJnz = inst == "jnz"
         regs = argsToRegs args
         regVals = [if fst (regs !! i) then registers !! (snd (regs !! i)) else snd (regs !! i) | i <- [0..(length regs -1)]]
         newRegs = -- {{{
           if isInc then
             [if i == (snd . last) regs then (registers !! i) + 1 else registers !! i | i <- [0..3]]
           else if isDec then
             [if i == (snd . last) regs then (registers !! i) - 1 else registers !! i | i <- [0..3]]
           else if isCpy then
             [if i == (snd . last) regs then head regVals else registers !! i | i <- [0..3]]
           else -- must be jnz
             registers-- }}}
         newPos = -- {{{
           if isJnz
             then if head regVals > 0 -- if first value is greater than 0
               then pos + last regVals
               else pos+1
             else pos+1 -- otherwise pos+1}}}}}}}}}


main = do
         lines <- readFile "input12.txt"-- {{{
         let instructions = (0, linesToList lines)
         let finalRegs = performInstructions instructions [0,0,1,0]
         print finalRegs-- }}}
