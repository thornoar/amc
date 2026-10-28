{-# LANGUAGE StandaloneDeriving #-}

module Object.Bundle where
import Object.RealNumber
import Description

-- data NumberSystem = ZZ | QQ | RR | Boolean
data ObjectTag = IEX | REX | BEX | STR deriving (Read, Show, Enum, Bounded)

allObjectTags :: [ObjectTag]
allObjectTags = enumFrom minBound

showObjectTag :: ObjectTag -> String
showObjectTag IEX = "integer expression"
showObjectTag REX = "real expression"
showObjectTag STR = "raw string"
showObjectTag BEX = "boolean expression"

instance Description IEX where description _ = "an integer expression"
instance Description REX where description _ = "a real number expression"
instance Description BEX where description _ = "a boolean expression"
instance Description STR where description _ = "a raw string"

instance Description ObjectTag where description _ = "an object tag"

data Object tg where
  -- Integer arithmetic
  IConst :: Integer -> Object IEX
  INeg :: Object IEX -> Object IEX
  ISum :: Object IEX -> Object IEX -> Object IEX
  IProd :: Object IEX -> Object IEX -> Object IEX
  IDiv :: Object IEX -> Object IEX -> Object IEX
  IMod :: Object IEX -> Object IEX -> Object IEX
  IPow :: Object IEX -> Object IEX -> Object IEX
  IVar :: String -> Object IEX

  -- Real number arithmetic
  RConst :: RealNumber -> Object REX
  RNeg :: Object REX -> Object REX
  RSum :: Object REX -> Object REX -> Object REX
  RInv :: Object REX -> Object REX
  RProd :: Object REX -> Object REX -> Object REX
  RDiv :: Object REX -> Object REX -> Object REX
  RLog :: Object REX -> Object REX -> Object REX
  RPow :: Object REX -> Object REX -> Object REX
  RVar :: String -> Object REX
  RApp :: BuiltinFunction -> Object REX -> Object REX

  Raw :: String -> Object STR

  BConst :: Bool -> Object BEX
  BVar :: String -> Object BEX
  BNot :: Object BEX -> Object BEX
  BAnd :: Object BEX -> Object BEX -> Object BEX
  BOr :: Object BEX -> Object BEX -> Object BEX
  BXor :: Object BEX -> Object BEX -> Object BEX
  BImpl :: Object BEX -> Object BEX -> Object BEX
  BEquiv :: Object BEX -> Object BEX -> Object BEX

deriving instance Show (Object tg)
deriving instance Eq (Object tg)
