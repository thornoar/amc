{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Simplify (SimplifyResult, simplifyResult) where

import Object
import Result (Result (..))
import Data.Kind (Constraint)
import Description

type Simplify :: ObjectTag -> Constraint
class Simplify tg where
  simplify :: Object tg -> Object tg

collectConst :: ([Integer], [Object IEX]) -> [Object IEX] -> ([Integer], [Object IEX])
collectConst (acc1, acc2) [] = (acc1, acc2)
collectConst (acc1, acc2) ((IConst v) : rest) = collectConst (v : acc1, acc2) rest
collectConst (acc1, acc2) (expr : rest) = collectConst (acc1, expr : acc2) rest

instance {-# OVERLAPPING #-} SimplifyResult IEX
instance Simplify IEX where
  simplify (IConst v) = IConst v
  simplify (INeg (INeg e)) = simplify e
  simplify (INeg (ISum es)) = simplify (ISum (map INeg es))
  simplify (INeg expr) = case simplify expr of
    IConst v -> IConst (-v)
    expr' -> INeg expr'
  simplify (ISum es) =
    let (consts, exprs) = collectConst ([], []) (map simplify es)
     in if length exprs == 0 then IConst (sum consts) else ISum ((IConst $ sum consts) : exprs)
  simplify (IProd es) =
    let (consts, exprs) = collectConst ([], []) (map simplify es)
     in if length exprs == 0 then IConst (product consts) else IProd ((IConst $ product consts) : exprs)
  simplify (IDiv e1 e2) = case (simplify e1, simplify e2) of
    (IConst v1, IConst v2) -> IConst (v1 `div` v2)
    (e1', e2') -> IDiv e1' e2'
  simplify (IMod e1 e2) = case (simplify e1, simplify e2) of
    (IConst v1, IConst v2) -> IConst (v1 `mod` v2)
    (e1', e2') -> IMod e1' e2'
  simplify (IPow e1 e2) = case (simplify e1, simplify e2) of
    (IConst v1, IConst v2) -> IConst (v1 ^ v2)
    (e1', e2') -> IPow e1' e2'
  simplify (IVar name) = IVar name

type SimplifyResult :: ObjectTag -> Constraint
class SimplifyResult tg where
  simplifyResult :: Object tg -> Result (Object tg)
  default simplifyResult :: Simplify tg => Object tg -> Result (Object tg)
  simplifyResult = Content . simplify
instance {-# OVERLAPPABLE #-} Description tg => SimplifyResult tg where
  simplifyResult obj = Error $ (description (proxyOf obj)) ++ " cannot be simplified"
