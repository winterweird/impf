module Thread where -- Type Thread and related functions are found here

import Syntax
import Data.Maybe (fromJust)

-- Custom data type defining a Thread tree
-- Arguments: tid, current ast, environment and context, list of child threads
--
-- Since all threads are dependent on the main thread, detach will put the
-- thread in the main thread's list of children and set its tid to Nothing to
-- show that it's detached. The main thread has no tid (the tid argument is
-- Nothing). The behavior of a thread completing is to drop all its children.
--
-- Join will make a thread not perform any steps until the thread it's joining
-- with has ended. If the TID is invalid, join will do nothing, since it assumes
-- the thread simply has ended.
--
-- Spawn puts a new Thread in the list of children threads.
data Thread = Thread { tid :: Tid
                     , ast :: Ast
                     , env :: Env
                     , ctx :: [Ctx]
                     , children :: [Thread]
                     } deriving Show

data Tid = MainThread 
         | DetachedThread
         | Tid Int
         deriving Show

instance Eq Thread where
  Thread (Tid tid1) _ _ _ _ == Thread (Tid tid2) _ _ _ _ = tid1 == tid2
  _ == _ = False -- just assume all other threads are unequal

-- given a tid and a parent thread, return a thread tree exluding the thread
-- with the given tid
excluding :: Thread -> Thread -> Thread
excluding thread1 thread2 | thread1 == thread2 = error "Implementation error: cannot exclude self"
excluding thread parent = let cs = filter (/=thread) (children parent) in
    parent{children = map (excluding thread) cs}
    
-- helper function: return the thread where all subthreads to any level with the
-- same tid as the new thread is replaced
replacing :: Thread -> Thread -> Thread
replacing newThread oldThread | newThread == oldThread = newThread
replacing thread parent = parent{children=map (replacing thread) (children parent)}

-- helper function: return whether a thread is finished running or not
completed :: Thread -> Bool
completed (Thread _ SSkip _ [] _) = True
completed _ = False

-- Helper function: given the main thread, get the next tid
-- TODO: update to use getChildren instead of pattern matching
-- Also, I'm never actually using tid...
-- I think this is broken; TODO: fix
nextTid :: Thread -> Int
nextTid t = 1 + (maxTid 0 t) 
    where maxTid n thread | tid thread == MainThread || tid thread == DetachedThread = n
                          | null $ children thread = max n (tid thread) 
                          | otherwise = max n (maximum $ map (maxTid n) (children thread))

-- helper function: find the thread with the given tid if it exists in the
-- thread tree, or otherwise Nothing
findThread :: Int -> Thread -> Maybe Thread
findThread tid1 t
  | (Just tid1) == tid t = Just t
  | null $ children t = Nothing
  | otherwise = case findThread tid1 (head $ children t) of

        Nothing -> findThread tid1 t{children=tail$children t}
        x -> x
