module Eval where -- how to execute/step logic lives here

import Syntax
import Primitive
import Pretty
import Thread
import Debug.Trace (trace)

import Control.Monad
import Data.IORef

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env =
  let (Just v) = lookup s env in v -- assumes that a variable is always found

exec :: Ast -> IO ()
exec e = steps [Thread e primitives [] 0 Nothing]

steps :: [Thread] -> IO ()
steps [] = error "Implementation error: No threads"
steps ((Thread SSkip _ [] 0 Nothing):ts) = return ()
steps st@(t:ts) = case (ast t, ctx t) of
    -- case: thread completed
    (SSkip, []) -> do -- remove children threads
        steps $ filter ((/=(Just $ tid t)) . parent) ts
    
    -- case: spawning a new thread
    (v@(EVal (VClosure [] _ cloEnv)), ESpawn Hole : ctx) -> do
        let nextTid = 1 + (maximum $ map tid st) 
        let newThread = Thread (SExpr $ ECall v [] []) cloEnv [] nextTid (Just $ tid t)
        steps $ (newThread:t{ast=EVal (VInt nextTid), ctx=ctx}:ts)

    -- case: trying to spawn thread with function taking arguments
    (EVal (VClosure _ _ _), ESpawn Hole : _) -> do
        error "Cannot spawn thread using non-nullary function"

    -- case: detaching a thread
    (EVal (VInt threadId), EDetach Hole : ctx) -> do
        let thisThread = t{ast=EVal VVoid, ctx=ctx} -- evaluate step in this thread
        
        let matching = filter ((==threadId) . tid) st -- all threads with tid = threadId
        
        if length matching /= 1 then do
            error $ "Couldn't detach unique thread with id " ++ (show threadId)
        else do
            let detachedThread = head $ matching -- should be just one
            let remaining = map (\x -> if x == thisThread then thisThread else x) $
                            filter (/=detachedThread) st
            steps $ detachedThread{parent=Nothing}:remaining

    -- case: joining a thread
    (EVal (VInt threadId), EJoin Hole : ctx) -> do
        let matching = filter ((==threadId) . tid) st -- all threads with given tid
        if length matching > 0 then steps $ ts ++ [t] -- don't advance this thread
        else steps $ ts ++ [t{ast=EVal VVoid, ctx=ctx}]
        
    -- case: anything else
    _ -> do
        (ast', env', ctx') <- step (ast t, env t, ctx t)
        steps $ ts++[t{ast=ast', env=env', ctx=ctx'}] -- put thread in the back of queue

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, SExpr Hole : ctx)
step (v, env, SExpr Hole : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, (SBlock (HoleWithEnv env)) : ctx)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, SSeq Hole s2 : ctx)
step (SSkip, env, SSeq Hole s2 : ctx) = return (s2, env, ctx) -- next step in sequence

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, SIf Hole s1 s2 : ctx)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx) = return (SBlock s1, env, ctx)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx) = return (SBlock s2, env, ctx)

step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, SVarDecl s Hole : ctx)
step (v, env, SVarDecl s Hole : ctx) | isValue v 
  = return (SSkip, addVar s (expr2val v) env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, SAssign s Hole : ctx)
step (v, env, SAssign s Hole : ctx) | isValue v = do
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""
  

-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, ERef Hole : ctx)
step (v, env, ERef Hole : ctx) | isValue v = do
  nv <- newIORef (expr2val v)
  return (EVal (VRef nv), env, ctx)

-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, EDeref Hole : ctx)
step (v, env, EDeref Hole : ctx) | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return $ (EVal v', env, ctx)

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Negation of an integer value
step (ENegation e, env, ctx) = return (e, env, ENegation Hole : ctx)
step ((EVal (VInt x)), env, ENegation Hole : ctx) = return (EVal (VInt (-x)), env, ctx)

-- NOTE: Put these here to have them near the ECall definition

-- Reset expression: evaluate the function to a closure
step (EReset f, env, ctx) = return (f, env, EReset Hole : ctx)
step (v@(EVal (VClosure pars s cloEnv)), env, EReset Hole : ctx) = do
  -- just call the function, but put a marker in the context
  return (ECall v [] [], env, EReset (HoleWithEnv env) : ctx) -- keep the EReset as a marker
step (EVal v, env, EReset _ : ctx) = do
  -- pop off the marker
  return (EVal v, env, ctx)

-- Shift expression: evaluate the function to a closure
step (EShift f, env, ctx) = return (f, env, EShift Hole : ctx)
step (v@(EVal (VClosure [x] s cloEnv)), env, EShift Hole : ctx) = do
  let cont = VCont cloEnv ctx
  let contEnv = addVar x cont env -- put the continuation param in env
  return (ECall v [EVar x] [], contEnv, EShift Hole : ctx)
step (v@(EVal (VCont contEnv contCtx)), env, EShift Hole : ctx) = do
  let remainingCtx = tail $  dropWhile (\x -> case x of {EReset _ -> False; _ -> True}) ctx
  case head remainingCtx of
    ECall Hole args vs -> do
      return (ECall v args vs, env, tail remainingCtx)
    _ -> do
      error "There was a problem unwinding the shift/reset stack"

-- Call with continuation and parameter
step (ECall (EVal cont@(VCont contEnv contCtx)) [] [a], env, ctx) = do
  (cont', env', ctx') <- step (EVal a, contEnv, contCtx)
  return (ECall (EVal (VCont env' ctx')) [] [], env, cont' : ctx)
-- Call with continuation and no parameter, but last part of continuation is at
-- head of ctx
step (ECall (EVal cont@(VCont contEnv contCtx@(x:xs))) [] [], env, a : ctx) = case (a, x) of
  (_, SBlock _) -> do
    -- skip over the part of return that will clobber my env
    return (ECall (EVal (VCont contEnv xs)) [] [], env, a : ctx)
  (EVal v, SReturn _) -> do
    -- return value to shift context
    return (EVal v, env, ctx)
  _ -> do
    -- take a step in continuation context
    (cont', env', ctx') <- step (a, contEnv, contCtx)
    return (ECall (EVal (VCont env' ctx')) [] [], env, cont' : ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) = do
  return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx)
step (EVal v, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal v, env, ctx)
  -- return statement executed in function
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal VVoid, env, ctx)
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx) = do
  return (EVal $ f (reverse vs), env, ctx)
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx)
step (ECall f [] _, _, _) | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function position
step (ECall f args [], env, ctx) | notValue f = return (f, env, ECall Hole args [] : ctx)
step (f, env, ECall Hole args [] : ctx) | isValue f = return (ECall f args [], env, ctx)
step (ECall f (a:args) vs, env, ctx) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx)
step (v, env, ECall f (Hole:args) vs : ctx) | isValue v = return (ECall f args (expr2val v : vs), env, ctx)

-- Return expression: evaluate to a value
step (SReturn v, _, SBlock (HoleWithEnv env) : ctx) = return (SReturn v, env, ctx) -- let the block return the value
step (SReturn v, env, SSeq Hole s2 : ctx) = return (SReturn v, env, ctx) -- skip the rest of the sequence
step (SReturn e, env, ctx) = return (e, env, SReturn Hole : ctx)
step (v, env, SReturn Hole : ctx) | isValue v = return (v, env, ctx)
step (v, _, SReturn Hole : _) = error $ "trying to return non-value " ++ (show v)

-- Try-catch statement: attempt to evaluate try, or evaluate the catch block
step (STry t c f exName, env, ctx) | notValue t = do
  return (t, env, STry Hole c f exName : ctx)
step (SSkip, env, STry Hole c f exName : ctx) = do
  return (f, env, STry SSkip c Hole exName : ctx) -- execute finally
step (val@(EVal v), env, STry Hole c f var@(EVar exName) : ctx) = do
  return (c, addVar exName v env, STry val (HoleWithEnv env) f var : ctx)
step (SSkip, _, STry t (HoleWithEnv env) f exName : ctx) = do
  return (f, env, STry t SSkip Hole exName : ctx) -- execute finally
step (SSkip, env, STry t c Hole exName : ctx) = do
  return (SSkip, env, ctx) -- finally executed

-- Throw statement: evaluate expression, then unwrap until next STry
step (SThrow v, _, SBlock (HoleWithEnv env) : ctx) = return (SThrow v, env, ctx) -- exit block
step (SThrow v, env, SSeq Hole s2 : ctx) = return (SThrow v, env, ctx)
step (SThrow e, env, ctx) = return (e, env, SThrow Hole : ctx)
step (v, env, SThrow Hole : ctx) | isValue v = return (v, env, ctx) -- proceed to catch

-- Break statement: unwind context until end of while, then drop while
step (SBreak, env, SSeq _ (SWhile _ _) : ctx) = return (SSkip, env, ctx)
step (SBreak, env, _ : ctx) = return (SBreak, env, ctx)

-- Continue statement: unwind context until end of while, then pick it up again
step (SContinue, env, SSeq Hole w@(SWhile _ _) : ctx) = return (SSeq SSkip w, env, ctx)
step (SContinue, env, _ : ctx) = return (SContinue, env, ctx)

-- Spawn expression: compute the closure, and spawn new thread in steps
step (ESpawn f, env, ctx) = return (f, env, ESpawn Hole : ctx)
step (EVal (VClosure _ _ _), _, ESpawn Hole : _) = error "Spawn should be handled in steps"

-- Detach expression: compute the tid, and detach in steps
step (EDetach e, env, ctx) = return (e, env, EDetach Hole : ctx)
step (EVal (VInt _), _, EDetach Hole : _) = error "Detach should be handled in steps"

-- Join expression: compute the tid, and join in steps
step (EJoin e, env, ctx) = return (e, env, EJoin Hole : ctx)
step (EVal (VInt _), _, EJoin Hole : _) = error "Join should be handled in steps"






step (a, b, c) = error $ "Nonexhaustive pattern: \n" ++ (show a) ++ "\n" ++ (show b) ++ "\n" ++ (show c)
