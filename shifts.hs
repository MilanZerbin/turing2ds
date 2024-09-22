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
printImg n img = concat (map printWindow (take n img))


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


----------- Impure Stuff and MAIN: 
-- Method to grab an Int from STDIN
getDim :: IO Int
getDim = do
    line <- getLine
    if null line
        then getDim
    else do
        let dim = (read line :: Int)
        return dim

-- Main loop 
main = do
    putStrLn "Please enter the Width of the Window you want to see from the Infinite Sequence:"
    windowwidth <- getDim -- obtain width
    putStrLn "Please enter how many Iterations you wish to see"
    iterlength <- getDim -- obtain depth
    putStrLn "------ Showing Computer (each Iteration one Line) --------"
    putStr (printImg iterlength (buildImg windowwidth exampleIterates)) 

