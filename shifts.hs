module ShiftDynamics where

data Window = Window [Bool] [Bool] deriving (Show)
data BiInfSeq = BiInfSeq [Bool] [Bool] 
data StepsizeFunction = StepsizeFunction (BiInfSeq -> Int)
data ModifyFunction = ModifyFunction (BiInfSeq -> BiInfSeq)
data ShiftComputer = ShiftComputer StepsizeFunction ModifyFunction

takeWindow :: Int -> BiInfSeq -> Window
takeWindow n (BiInfSeq l r) = Window (take n l) (take n r)

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
applyShift (ShiftComputer g f) b = step (g b) (f b)

constructF :: (Window -> Window) -> ModifyFunction

