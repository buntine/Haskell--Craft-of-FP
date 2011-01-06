
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.

-- 	The queue ADT: its signature is given in comments in the module
-- 	header.

module QueueState 

  ( QueueState ,
    addMessage,      -- Inmess -> QueueState -> QueueState
    queueStep,       -- QueueState -> ( QueueState , [Outmess] )
    queueStart,      -- QueueState
    queueLength,     -- QueueState -> Int
    queueEmpty       -- QueueState -> Bool
    ) where

import Base		-- for the base types of the system

type Time = Int

-- The implementation of the QueueState, where the first field gives the 
-- current time, the second the service time so far for the item currently 
-- being processed,

data QueueState = QS Time Service [Inmess]
                  deriving (Eq, Show)

-- To add a message, it is put at the end of the list of messages.

addMessage  :: Inmess -> QueueState -> QueueState

addMessage im (QS time serv ml) = QS time serv (ml++[im])

-- A single step in the queue simulation.

queueStep   :: QueueState -> ( QueueState , [Outmess] )

queueStep (QS time  servSoFar (Yes arr serv : inRest))
  | servSoFar < serv
    = (QS (time+1) (servSoFar+1) (Yes arr serv : inRest) , [])
  | otherwise
    = (QS (time+1) 0 inRest , [Discharge arr (time-serv-arr) serv])
--  
queueStep (QS time serv []) = (QS (time+1) serv [] , [])

-- The starting state

queueStart  :: QueueState
queueStart  =  QS 0 0 [] 

-- The length of the queue

queueLength :: QueueState -> Int
queueLength (QS _ _ q) = length q

-- Is the queue empty?

queueEmpty  :: QueueState -> Bool
queueEmpty (QS _ _ q)  = (q==[])


