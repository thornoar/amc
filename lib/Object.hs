{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Object where
import RealNumber

data NumberSystem = ZZ | QQ | RR | Boolean

data ObjectTag = EXP

data Object t ns where
  -- Integer arithmetic
  ZConst :: Integer -> Object EXP ZZ
  ZNeg :: Object EXP ZZ -> Object EXP ZZ
  ZSum :: [Object EXP ZZ] -> Object EXP ZZ
  -- ZDiff :: Object EXP ZZ -> Object EXP ZZ -> Object EXP ZZ
  ZProd :: [Object EXP ZZ] -> Object EXP ZZ
  ZDiv :: Object EXP ZZ -> Object EXP ZZ -> Object EXP ZZ
  ZMod :: Object EXP ZZ -> Object EXP ZZ -> Object EXP ZZ
  ZPow :: Object EXP ZZ -> Object EXP ZZ -> Object EXP ZZ
  ZVar :: String -> Object EXP ZZ

  -- Real number arithmetic
  RConst :: RealNumber -> Object EXP RR
  RNeg :: Object EXP RR -> Object EXP RR
  RInv :: Object EXP RR -> Object EXP RR
  RLn :: Object EXP RR -> Object EXP RR
  RSum :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RDiff :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RProd :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RDiv :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RPow :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RLog :: Object EXP RR -> Object EXP RR -> Object EXP RR
  RVar :: String -> Object EXP RR
