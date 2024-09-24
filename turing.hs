module TuringMachines where

-- Define a single (infinite) Band Turing Machine data Type. 
--  The other Band is just for avoiding keeping Tabs on where the Head is at
data MachineBand = MachineBand [Maybe Bool] [Maybe Bool]
--  The Head carries a State
data State = State Int
-- The entire State is given by the MachineBand and State of the Head
data MachineState = MachineState State MachineBand
data TuringMachine = Turingmachine StateUpdate RightOrLeft WriteFunction
-- The three Functions that make up a Turing Machine movement
data WriteFunction = WriteFunction State -> Maybe Bool -> Maybe Bool
data RightOrLeft = RightOrLeft Maybe Bool -> State -> Maybe Bool
data StateUpdate = StateUpdate Maybe Bool -> State -> State

-- Method that performes one Move of the TuringMachine
stepOnce :: TuringMachine -> MachineState -> MachineState
stepOnce (TuringMachine k s m w) (MachineState (State i) (MachineBand l (r:rs))) = MachineState (s r i) moveHead (m r i) (MachineBand l : ((w i r):rs)) 

-- doesnt move on nothing, moves right on True, and left on false
moveHead :: Maybe Bool -> MachineBand -> MachineBand
moveHead Nothing _ = _
moveHead Just False (MachineBand (l:ls) rs) = MachineBand ls (r:rs)
moveHead Just True (MachineBand ls (r:rs)) = MachineBand (r:ls) rs 

