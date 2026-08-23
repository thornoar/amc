{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Simplify where

import Object
-- import RealNumber

class Simplify a where
  simplify :: a -> a

collectConst :: ([Integer], [Object EXP ZZ]) -> [Object EXP ZZ] -> ([Integer], [Object EXP ZZ])
collectConst (acc1, acc2) [] = (acc1, acc2)
collectConst (acc1, acc2) ((ZConst v) : rest) = collectConst (v : acc1, acc2) rest
collectConst (acc1, acc2) (expr : rest) = collectConst (acc1, expr : acc2) rest

instance Simplify (Object EXP ZZ) where
  simplify (ZConst v) = ZConst v
  simplify (ZNeg (ZNeg e)) = simplify e
  simplify (ZNeg (ZSum es)) = simplify (ZSum (map ZNeg es))
  simplify (ZNeg expr) = case simplify expr of
    ZConst v -> ZConst (-v)
    expr' -> ZNeg expr'
  simplify (ZSum es) =
    let (consts, exprs) = collectConst ([], []) (map simplify es)
     in if length exprs == 0 then ZConst (sum consts) else ZSum ((ZConst $ sum consts) : exprs)
  simplify (ZProd es) =
    let (consts, exprs) = collectConst ([], []) (map simplify es)
     in if length exprs == 0 then ZConst (product consts) else ZProd ((ZConst $ product consts) : exprs)
  simplify (ZDiv e1 e2) = case (simplify e1, simplify e2) of
    (ZConst v1, ZConst v2) -> ZConst (v1 `div` v2)
    (e1', e2') -> ZDiv e1' e2'
  simplify (ZMod e1 e2) = case (simplify e1, simplify e2) of
    (ZConst v1, ZConst v2) -> ZConst (v1 `mod` v2)
    (e1', e2') -> ZMod e1' e2'
  simplify (ZPow e1 e2) = case (simplify e1, simplify e2) of
    (ZConst v1, ZConst v2) -> ZConst (v1 ^ v2)
    (e1', e2') -> ZPow e1' e2'
  simplify (ZVar name) = ZVar name
