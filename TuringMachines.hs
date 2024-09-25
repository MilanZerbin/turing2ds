module TuringMachines where

-- Define a single (infinite) Band Turing Machine data Type. 
--  The other Band is just for avoiding keeping Tabs on where the Head is at
data MachineBand = MachineBand [Maybe Bool] [Maybe Bool]
--  The Head carries a State
data State = State (Maybe Bool)
-- The entire State is given by the MachineBand and State of the Head
data MachineState = MachineState State MachineBand
data TuringMachine = TuringMachine StateUpdate RightOrLeft WriteFunction
-- The three Functions that make up a Turing Machine movement
data WriteFunction = WriteFunction (State -> Maybe Bool -> Maybe Bool)
data RightOrLeft = RightOrLeft (Maybe Bool -> State -> Maybe Bool)
data StateUpdate = StateUpdate (Maybe Bool -> State -> State)

tellState :: State -> Maybe Bool
tellState (State i) = i
 
-- Method that performes one Computational Move of the TuringMachine
--  consists the Head looking at the tape, deciding its next move, and writing at that position 
stepOnce :: TuringMachine -> MachineState -> MachineState
stepOnce (TuringMachine (StateUpdate s) (RightOrLeft m) (WriteFunction w)) (MachineState i (MachineBand l (r:rs))) = MachineState (s r i) (moveHead (m r i) (MachineBand l ((w i r):rs))) 

-- doesnt move on nothing, moves right on True, and left on false
moveHead :: Maybe Bool -> MachineBand -> MachineBand
moveHead (Nothing) b = b
moveHead (Just False) (MachineBand (l:ls) rs) = MachineBand ls (l:rs)
moveHead (Just True) (MachineBand ls (r:rs)) = MachineBand (r:ls) rs 

-- Initializes a MachineState with given Starting Conditions
--  Headstate gets set to Nothing
-- Input is the String written initially on the Tape
initMState :: [Bool] -> MachineState
initMState r = MachineState (State Nothing) (MachineBand l rs) where
    l = [Nothing | k <- [1..]]
    rs = (map pure r) ++ [Nothing | k <- [1..]]

