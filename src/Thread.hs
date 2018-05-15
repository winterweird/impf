module Thread where

import Syntax

-- All threads assume main thread is 0.
-- Main thread and detached threads have parent = Nothing (?)
data Thread = Thread { ast :: Ast
                     , env :: Env
                     , ctx :: [Ctx]
                     , tid :: Int
                     , parent :: Maybe Int
                     } deriving Show

instance Eq Thread where
    t1 == t2 = tid t1 == tid t2
