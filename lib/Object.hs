{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Object where
import RealNumber
import Description

-- data NumberSystem = ZZ | QQ | RR | Boolean

data ObjectTag = IEX | REX | STR deriving (Read, Show)

instance Description IEX where description _ = "an integer expression"
instance Description REX where description _ = "a real number expression"
instance Description STR where description _ = "a string"

data Object tg where
  -- Integer arithmetic
  IConst :: Integer -> Object IEX
  INeg :: Object IEX -> Object IEX
  ISum :: [Object IEX] -> Object IEX
  -- IDiff :: Object IEX -> Object IEX -> Object IEX
  IProd :: [Object IEX] -> Object IEX
  IDiv :: Object IEX -> Object IEX -> Object IEX
  IMod :: Object IEX -> Object IEX -> Object IEX
  IPow :: Object IEX -> Object IEX -> Object IEX
  IVar :: String -> Object IEX

  -- Real number arithmetic
  RConst :: RealNumber -> Object REX
  RNeg :: Object REX -> Object REX
  RInv :: Object REX -> Object REX
  RLn :: Object REX -> Object REX
  RSum :: Object REX -> Object REX -> Object REX
  RDiff :: Object REX -> Object REX -> Object REX
  RProd :: Object REX -> Object REX -> Object REX
  RDiv :: Object REX -> Object REX -> Object REX
  RPow :: Object REX -> Object REX -> Object REX
  RLog :: Object REX -> Object REX -> Object REX
  RVar :: String -> Object REX

  Raw :: String -> Object STR

deriving instance Show (Object tg)
