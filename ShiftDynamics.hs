module ShiftDynamics where

--------- Define Types: 
-- Window: Consists of a Length and two boolean Lists of that length.
--          is used to work on finite patches of BiInfinite Sequences.
-- BiInfSeq: Consists of a left and right infinite boolean List.
--          is used to save the state of the ShiftComputer
-- StepsizeFunction: The function that calculates how much to step.
--          called "G" in Moores paper
-- ModifyFunction: The function that modifies a Window in the BiInfSeq
--          called "F" in Moores paper
-- ShiftComputer: Consists of a StepsizeFunction and a ModifyFunction.
--          used in the computation of the next step of the ShiftDynamics
data Window = Window Int [Bool] [Bool] deriving (Show)
data BiInfSeq = BiInfSeq [Bool] [Bool] 
data StepsizeFunction = StepsizeFunction (BiInfSeq -> Int)
data ModifyFunction = ModifyFunction (BiInfSeq -> BiInfSeq)
data ShiftComputer = ShiftComputer StepsizeFunction ModifyFunction

-------- Basic Utilities:
-- Grab the right List of a BiInfSeq
takeRight :: BiInfSeq -> [Bool]
takeRight (BiInfSeq _ r) = r

-- Grab a size n window from the middle of a bi-infite Sequence.
takeWindow :: Int -> BiInfSeq -> Window
takeWindow n (BiInfSeq l r) = Window n (take n l) (take n r)

-- Move the "decimal point" one step to the left
stepright :: BiInfSeq -> BiInfSeq
stepright (BiInfSeq ls (r:rs)) = BiInfSeq (r:ls) rs

-- Move the "decimal point" one step to the right
stepleft :: BiInfSeq -> BiInfSeq
stepleft (BiInfSeq (l:ls) rs) = BiInfSeq ls (l:rs)

-- Move the "decimal point" n steps to the right (left when n is negative)
step :: Int -> BiInfSeq -> BiInfSeq
step n b 
    | n > 0 = step (n-1) (stepright b)
    | n < 0 = step (n+1) (stepleft b)
    | otherwise = b

-- Patch in a size n window around the comma in the middle of a BIS
glueWindow :: Window -> BiInfSeq -> BiInfSeq
glueWindow (Window n l r) (BiInfSeq ls rs) = BiInfSeq (l ++ ls) (r ++ rs)

-- Delete a size n window around the comma in a BIS
cutWindow :: Int -> BiInfSeq -> BiInfSeq
cutWindow 0 b = b
cutWindow n (BiInfSeq (l:ls) (r:rs)) = cutWindow (n-1) (BiInfSeq ls rs)

-- Give a Constructor for obtaining ModifyFunctions.
--  the Integer n stands for the Windowsize the Function is allowed to see.
--  It cuts a Window of size n from the middle, modifies it, 
--  then paste it back in
constructF :: Int -> (Window -> Window) -> ModifyFunction
constructF n f = ModifyFunction hatf where
    hatf b = glueWindow newWindow outerPart where
        newWindow = f (takeWindow n b)
        outerPart = cutWindow n b

-- Give a Constructor for obtaining StepsizeFunctions.
--  Has no use at the moment, might have later
constructG :: (BiInfSeq -> Int) -> StepsizeFunction
constructG g = StepsizeFunction g


--------- Actual Shiftcomputer:
-- looks at a chunk in the middle, calculate stepsize from that, 
-- then modify it and step accordingly
applyShift :: ShiftComputer -> BiInfSeq -> BiInfSeq
applyShift (ShiftComputer (StepsizeFunction g) (ModifyFunction f)) b = step (g b) (f b)

-- creates an infinite List of all the iterations of a ShiftComputer
-- needs a starting condition in form of a BIS
evolve :: ShiftComputer -> BiInfSeq -> [BiInfSeq]
evolve c b = iterate (applyShift c) b


---------- Methods to represent or animate:
-- How should we draw the boolean Values on the Screen?
--   Represents True as " " and False as a black block.
reprBool :: Bool -> String
reprBool o 
    | o = " "
    | otherwise = "■"

-- Implements "Show"-Instance for a Window. 
--  Takes the left list, reverses it and glues it to the right.
--  Puts the "comma separator" "|" in between, and "newline" at the end.
printWindow :: Window -> String
printWindow (Window n l r) = concat ([reprBool (l !! (n - k)) | k <- [1..n]] ++ ["|"] ++ (map reprBool r) ++ [" \n "])

-- look at only a window of size n in the List of Iterations
buildImg :: Int -> [BiInfSeq] -> [Window]
buildImg n = map (takeWindow n)

-- Grabs n iterations from the already cut List of Iterations,
--  then converts them to Strings via printWindow
printImg :: Int -> [Window] -> String
printImg n img = " " ++ concat (map printWindow (take n img))


----------- EXAMPLE 1 from Moores Paper:
preexampleF :: Window -> Window
preexampleF (Window n l r) = Window n (map not l) r

preexampleG :: BiInfSeq -> Int
preexampleG (BiInfSeq (l:ls) rs) 
    | l = 1
    | otherwise = (-1)

exampleF = constructF 1 preexampleF
exampleG = constructG preexampleG

exampleShiftComputer = ShiftComputer exampleG exampleF

exampleStart = BiInfSeq ([(mod i 3) == 0 | i <- [1..]]) ([(mod i 4) == 0 | i <- [1..]])

exampleIterates = evolve exampleShiftComputer exampleStart

----------- Example Logic Gates (AND, XOR, ...):
-- Explanation of the Algorithm
-- given two finite boolean strings a and b, set up the tape:
--  get [a_1, a_2, ..., a_n], [b_1, ..., b_m] with n >= m
--      1. fill up b to the same length 
--          -> b = [b_(-n+m), ..., b_(-1) , b_1, ..., b_m] 
--          with b_(-i) = False
--      2. build a Window of Form
--          [True, a_1, b_(-m+n), True, a_2, b_(-m+n+1), ..., True, a_n, b_m]
--      3. Patch this into the BIS [True, ...] [False, ...]
--  think of the Tape consisting of Triplets, (state, aInfo, bInfo)
-- now the Algo works as follows:
--      1. Look at the triplet right of you: As long as you see "True" in the state
--          -> apply the logic gate X: let x = aInfo X bInfo
--          -> write x to aInfo and ¬x to bInfo
--          -> Move three to the right
--      2. As soon as your done ("False" in the state of your right triplet):
--          -> move three to the left,
--              as long as in your left triplet aInfo != bInfo
-- Afterwards, you will find "a X b" by reading off every third element
--  starting from the second from the right band. 
-- -> This algo terminates after 2n steps.

-- Fill a Window with Bool t on the side where it has less elmts
-- makes sure that n = len(a) = len(b) in the end
rectifyWith :: Bool -> Window -> Window
rectifyWith t (Window n l r)
    | ((length l) == n) && ((length r) == n) = Window n l r
    | (length l) < n = rectifyWith t (Window n (t:l) r)
    | (length r) < n = rectifyWith t (Window n l (t:r))
    | otherwise = rectifyWith t (Window (max (length l) (length r)) l r)

-- method to weave [t_1, ...] [a_1, ...] [b_1, ...]
--  into [t_1, a_1, b_1, ...]
interlace :: [Bool] -> [Bool] -> [Bool] -> [Bool]
interlace (t:ts) (a:as) (b:bs) = t:a:b:(interlace ts as bs)
interlace _ _ _ = []

-- method to build the right side of the needed band
-- takes Bool t to interlace with, and two boolean strings
foldAndInterlace :: Bool -> Window -> [Bool]
foldAndInterlace t (Window n a b) = interlace [t | i <- [1..n]] a b


-- build the tape according to step 2.
-- takes as input a Window with a and b saved in l and r.
buildTape :: Window -> BiInfSeq
buildTape (Window n a b) = BiInfSeq [True | i <- [1..]] rs where
    rs = rtriplets ++ [False | i <- [1..]] where
        rtriplets = foldAndInterlace True (rectifyWith False (Window n a b))

-- return the StepsizeFunction for the logic Gate ShiftComputer
preLogicG :: BiInfSeq -> Int
preLogicG (BiInfSeq (t:tt:ls) (r:rs)) 
    | r == True = 3
    | (t /= tt) && (r == False) = -3
    | otherwise = 0

-- take a logical Gate x and return preModifyFunction
--  for the corresponding logic Gate ShiftComputer
preLogicF :: (Bool -> Bool -> Bool) -> Window -> Window
preLogicF x (Window n l (t:ai:bi:rs))
    | t == True = Window n l (False:(x ai bi):(not (x ai bi)):rs)
    | otherwise = Window n l (t:ai:bi:rs)

-- makes one explicit shiftcomputer for "and" gate
logicF = constructF 3 (preLogicF (&&))
logicG = constructG preLogicG
logicSC = ShiftComputer logicG logicF

-- grabs the result of the logical SC from the tape
-- that means every third byte on the right tape,
--  starting on byte 2
-- up to a length of Int n
grabLogicSCResult :: Int -> BiInfSeq -> [Bool]
grabLogicSCResult n its = take n filteredits where
    filteredits = map fst (filter (\t -> ((snd t) == 2)) (zip (takeRight its) (cycle [1,2,3])))

----------- Impure Stuff and MAINS: 
-- Method to grab an Int from STDIN
getDim :: IO Int
getDim = do
    line <- getLine
    if null line
        then getDim
    else do
        let dim = (read line :: Int)
        return dim

-- Method to grab a Boolean String from STDIN
getBoolString :: [Bool] -> IO [Bool]
getBoolString a = do
    line <- getLine
    if null line
        then return [a!!((length a) - i) | i <- [1..(length a)]]
    else do
        let t = (read line :: Bool)
        getBoolString (t:a)

-- Main loop for the 1.st example from Moore
main = do
    putStrLn "Please enter the Width of the Window you want to see from the Infinite Sequence:"
    windowwidth <- getDim -- obtain width
    putStrLn "Please enter how many Iterations you wish to see"
    iterlength <- getDim -- obtain depth
    putStrLn "------ Showing Computer (each Iteration one Line) --------"
    putStr (printImg iterlength (buildImg windowwidth exampleIterates)) 

-- Main loop for the LogicGateSC:
logicmain = do
    putStrLn "Please enter the first string of true and false, separated by newlines:\n a = "
    a <- getBoolString []
    putStrLn "please enter the second string of true and false, again separated by newlines:\n b = "
    b <- getBoolString []
    putStrLn "here is the ShiftComputer computing (a AND b):"
    putStrLn "------ Showing Computer (each Iteration one Line) --------"
    let 
        n = max (length a) (length b) in
        let 
            iterlength = 2 * n + 3
            windowwidth = 3 * n + 6
            theIterates = evolve logicSC (buildTape (Window n a b)) in
            ((putStr (printImg iterlength (buildImg windowwidth theIterates))) >>
            (putStrLn "------ Computer Halted -----------------------------------")) >>
            ((putStrLn "It got the Result:") >>
            -- print every second element of the right list
            (putStrLn (show (grabLogicSCResult n (theIterates !! iterlength)))))
    
