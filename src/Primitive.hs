module Primitive where -- primitive functions (like arithmetic ops) live here

import Syntax
import Control.Monad

primitives = [
  ("__b+", VPrimFun $ \[x, y] -> case (x, y) of
    (VInt a, VInt b) -> VInt $ a + b
    (a, b) -> VString $ (show a) ++ (show b)), 
  ("__b+", VPrimFun $ \[x, y] -> VString $ (show x) ++ (show y)),
  ("__b-", VPrimFun $ \[VInt x, VInt y] -> VInt $ x - y), 
  ("__b*", VPrimFun $ \[VInt x, VInt y] -> VInt $ x * y), 
  ("__b/", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `div` y),
  ("__b%", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `mod` y),   
  ("__b==", VPrimFun $ \args -> case args of
      [VBool x, VBool y] -> VBool $ x == y
      [VInt x, VInt y] -> VBool $ x == y),
  ("__b!=", VPrimFun $ \args -> case args of
      [VBool x, VBool y] -> VBool $ x /= y
      [VInt x, VInt y] -> VBool $ x /= y),
  ("__b<", VPrimFun $ \[VInt x, VInt y] -> VBool $ x < y), 
  ("__b<=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x <= y), 
  ("__b>", VPrimFun $ \[VInt x, VInt y] -> VBool $ x > y), 
  ("__b>=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x >= y),

  ("__b&&", VPrimFun $ \[VBool x, VBool y] -> VBool $ x && y), 
  ("__b||", VPrimFun $ \[VBool x, VBool y] -> VBool $ x || y),

  ("print", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> return VVoid),
  ("println", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]
