module Conversion where

import ShiftDynamics
import TuringMachines

-- implements the conversion from TM's to SC's
convertState :: MachineState -> BiInfSeq
convertState (MachineState (State i) (MachineBand ls rs)) = BiInfSeq ((purify [i]) ++ (purify ls)) (purify rs)

-- a Method to take a Seq like
--  [Just True, Just False, Nothing, Just False, Nothing, ...]
-- and convert it to a pure sequence via doubling: 
--  map Just True -> True True, Just False -> False True
--  and Nothing -> False False.
--      The Shiftcomputer can then tell which entries were supposed to be "nothing".
purify :: [Maybe Bool] -> [Bool]
purify [] = []
purify (x:xs) 
    | x == Just True = (True:True:(purify xs))
    | x == Just False = (False:True:(purify xs))
    | x == Nothing = (False:False:(purify xs))

depurify :: (Bool, Bool) -> Maybe Bool
depurify (x,y)
    | y == False = Nothing
    | (x == True) && (y /= False) = Just True
    | (x == False) && (y /= False) = Just False

-- converts the left or right or nothing Instruction into 
--  moving Values the SC understands
decodeMove :: Maybe Bool -> Int
decodeMove x 
    | x == Nothing = 0
    | x == Just True = 2
    | x == Just False = -2


-- The conversion map from moores Paper (more or less)
--  Looks at the State (i,ii) and the current position (r,rr)
--      saved on the Band, converts them to State and MaybeBool again.
--  Then caculates the StepsizeFunction as what the RoL-Function tells
--  Then calculates the ModifyFunction as a sort of shuffle 
--      So that after moving the state is again in the same place.
convertComputer :: TuringMachine -> ShiftComputer
convertComputer (TuringMachine (StateUpdate s) (RightOrLeft m) (WriteFunction w)) = ShiftComputer (StepsizeFunction g) (ModifyFunction f) where
    g (BiInfSeq (i:ii:ls) (r:rr:rs)) = decodeMove (m (depurify (r,rr)) (State (depurify (i,ii))))
    f (BiInfSeq (i:ii:l:ll:ls) (r:rr:rs))
        | (m (depurify (r,rr)) (State (depurify (i,ii)))) == Just True =  BiInfSeq ((purify (map tellState [s (depurify (r,rr)) (State (depurify (i,ii)))])) ++ (purify [w (State (depurify (i,ii))) (depurify (r,rr))]) ++ (l:ll:ls)) (rs)
        | (m (depurify (r,rr)) (State (depurify (i,ii)))) == Just False  =  BiInfSeq ((purify (map tellState [s (depurify (r,rr)) (State (depurify (i,ii)))])) ++ ls) ([ll,l] ++ (purify [w (State (depurify (i,ii))) (depurify (r,rr))]) ++ rs)
        | (m (depurify (r,rr)) (State (depurify (i,ii)))) == Nothing  =  BiInfSeq ((purify (map tellState [s (depurify (r,rr)) (State (depurify (i,ii)))])) ++ (l:ll:ls)) ((purify [w (State (depurify (i,ii))) (depurify (r,rr))]) ++ rs)
--- REMARK : This code is ugly as fck, there is urgent need for a rework.
-- Maybe using Monads more, one could make it more readable?
-- Or introducing a "let... in " clause?
--  Anyways: TODO
