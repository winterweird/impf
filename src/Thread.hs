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
data Thread = Thread (Maybe Int) Ast Env [Ctx] [Thread] deriving Show

instance Eq Thread where
  Thread (Just tid1) _ _ _ _ == Thread (Just tid2) _ _ _ _ = tid1 == tid2
  _ == _ = False -- just assume all other threads are unequal

-- given a tid and a parent thread, return a thread tree exluding the thread
-- with the given tid
excluding :: Thread -> Maybe Int -> Thread
excluding t@(Thread tid2 _ _ _ _) (Just tid1) | (Just tid1) /= tid2 = fromJust $ excluding' tid1 t
  where excluding' tid (Thread (Just tid2) _ _ _ _) | tid == tid2 = Nothing
        excluding' tid (Thread tid' ast env ctx ch) = Just $ Thread tid' ast env ctx $ 
                   map fromJust $ filter (/=Nothing) $ map (excluding' tid) ch
excluding t _ = t -- else, just return self

nextTid :: Thread -> Int
nextTid t = 1 + (nextTid' 0 t)
    where nextTid' n (Thread tid _ _ _ children) = maximum $ n : (map (nextTid' n) children) 

-- helper function: get thread id of thread
getTid :: Thread -> Maybe Int
getTid (Thread tid _ _ _ _) = tid

-- helper function: get child processes of thread
getChildren :: Thread -> [Thread]
getChildren (Thread _ _ _ _ children) = children

-- helper function: find the thread with the given tid if it exists in the
-- thread tree, or otherwise Nothing
findThread :: Int -> Thread -> Maybe Thread
findThread tid1 t@(Thread tid2 ast env ctx children)
  | (Just tid1) == tid2 = Just t
  | null children = Nothing
  | otherwise = case findThread tid1 (head children) of
        Nothing -> findThread tid1 (Thread tid2 ast env ctx (tail children))
        x -> x

completed :: Thread -> Bool
completed (Thread _ SSkip _ [] _) = True
completed _ = False
