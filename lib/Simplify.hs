{-# LANGUAGE DataKinds #-}
module Simplify where

import Object
import RealNumber

-- evalInteger :: Object EXP ZZ -> Integer
-- evalInteger (ZConst v) = v
-- -- evalInteger (ZNeg )


collectConst :: ([Integer], [Object EXP ZZ]) -> [Object EXP ZZ] -> ([Integer], [Object EXP ZZ])
collectConst (acc1, acc2) [] = (acc1, acc2)
collectConst (acc1, acc2) ((ZConst v) : rest) = collectConst (v : acc1, acc2) rest
collectConst (acc1, acc2) (expr : rest) = collectConst (acc1, expr : acc2) rest

simplifyInteger :: Object EXP ZZ -> Object EXP ZZ
simplifyInteger (ZConst v) = ZConst v
simplifyInteger (ZNeg (ZNeg e)) = simplifyInteger e
simplifyInteger (ZNeg (ZSum es)) = simplifyInteger (ZSum (map ZNeg es))
-- simplifyInteger (ZNeg (ZDiff e1 e2)) = simplifyInteger (ZDiff e2 e1)
-- simplifyInteger (ZNeg (ZDiv e1 e2)) = simplifyInteger (ZDiv (ZNeg e1) e2)
simplifyInteger (ZNeg expr) = case simplifyInteger expr of
  ZConst v -> ZConst (-v)
  expr' -> ZNeg expr'
simplifyInteger (ZSum es) =
  let (consts, exprs) = collectConst ([], []) (map simplifyInteger es)
   in if length exprs == 0 then ZConst (sum consts) else ZSum ((ZConst $ sum consts) : exprs)
simplifyInteger (ZProd es) =
  let (consts, exprs) = collectConst ([], []) (map simplifyInteger es)
   in if length exprs == 0 then ZConst (product consts) else ZProd ((ZConst $ product consts) : exprs)
-- simplifyInteger (ZDiv e1 e2) = 
