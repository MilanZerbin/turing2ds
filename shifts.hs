module ShiftDynamics where

data Window = Window Int [Bool] [Bool] deriving (Show)
data BiInfSeq = BiInfSeq [Bool] [Bool] 
data StepsizeFunction = StepsizeFunction (BiInfSeq -> Int)
data ModifyFunction = ModifyFunction (BiInfSeq -> BiInfSeq)
data ShiftComputer = ShiftComputer StepsizeFunction ModifyFunction

takeWindow :: Int -> BiInfSeq -> Window
takeWindow n (BiInfSeq l r) = Window n (take n l) (take n r)

stepright :: BiInfSeq -> BiInfSeq
stepright (BiInfSeq ls (r:rs)) = BiInfSeq (r:ls) rs

stepleft :: BiInfSeq -> BiInfSeq
stepleft (BiInfSeq (l:ls) rs) = BiInfSeq ls (l:rs)

step :: Int -> BiInfSeq -> BiInfSeq
step n b 
    | n > 0 = step (n-1) (stepright b)
    | n < 0 = step (n+1) (stepleft b)
    | otherwise = b

applyShift :: ShiftComputer -> BiInfSeq -> BiInfSeq
applyShift (ShiftComputer (StepsizeFunction g) (ModifyFunction f)) b = step (g b) (f b)

glueWindow :: Window -> BiInfSeq -> BiInfSeq
glueWindow (Window n l r) (BiInfSeq ls rs) = BiInfSeq (l ++ ls) (r ++ rs)

cutWindow :: Int -> BiInfSeq -> BiInfSeq
cutWindow 0 b = b
cutWindow n (BiInfSeq (l:ls) (r:rs)) = cutWindow (n-1) (BiInfSeq ls rs)

constructF :: Int -> (Window -> Window) -> ModifyFunction
constructF n f = ModifyFunction hatf where
    hatf b = glueWindow newWindow outerPart where
        newWindow = f (takeWindow n b)
        outerPart = cutWindow n b

constructG :: (BiInfSeq -> Int) -> StepsizeFunction
constructG g = StepsizeFunction g

evolve :: ShiftComputer -> BiInfSeq -> [BiInfSeq]
evolve c b = iterate (applyShift c) b

buildImg :: Int -> [BiInfSeq] -> [Window]
buildImg n = map (takeWindow n)

reprBool :: Bool -> String
reprBool o 
    | o = " "
    | otherwise = "■"

printWindow :: Window -> String
printWindow (Window n l r) = concat ([reprBool (l !! (n - k)) | k <- [1..n]] ++ ["|"] ++ (map reprBool r) ++ [" \n "])

printImg :: Int -> [Window] -> String
printImg n img = concat (map printWindow (take n img))


-- start example 1 from moore
preexampleF :: Window -> Window
preexampleF (Window n l r) = Window n (map not l) r

exampleF = constructF 1 preexampleF
exampleG = constructG preexampleG

preexampleG :: BiInfSeq -> Int
preexampleG (BiInfSeq (l:ls) rs) 
    | l = 1
    | otherwise = (-1)

exampleShiftComputer = ShiftComputer exampleG exampleF

exampleStart = BiInfSeq ([(mod i 3) == 0 | i <- [1..]]) ([(mod i 4) == 0 | i <- [1..]])

exampleImg = buildImg 40 (evolve exampleShiftComputer exampleStart)
 
main = do
    putStr (printImg 200 exampleImg)


