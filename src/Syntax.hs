module Syntax where -- types (Ast, etc) are found here

import Data.IORef

data Ast =
    SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SVarDecl String Expr
  | SExpr Expr
  | SReturn Expr
  | STry Stmt Stmt Expr
  | SThrow Expr

  | EVal Value 
  | EVar String 
  | EFun [String] Stmt 
  | ECall Expr [Expr] [Value] 
  | ERef Expr 
  | EDeref Expr

  | Hole
  | HoleWithEnv Env
    deriving Show

type Stmt = Ast
type Expr = Ast
type Ctx = Ast

data Value = 
    VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value)
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)

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
data Thread = Thread (Maybe Int) Ast Env [Ctx] [Thread]

isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _ = False
notValue = not . isValue

expr2val :: Expr -> Value
expr2val (EVal v) = v

type Env = [(String, Value)]

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef _) = "ref"
  show (VVoid) = "void"
  show (VClosure _ _ _) = "closure"
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"
