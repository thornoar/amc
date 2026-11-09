module Cache.Bundle (Cache, cache) where
import Object.Bundle
import Data.Kind (Constraint)
import Object.RealNumber (cacheRN, cacheBF)

type Cache :: ObjectTag -> Constraint
class Cache tg where
  cache :: Object tg -> String

instance Cache STR where
  cache (Raw str) = str

instance Cache IEX where
  cache (IConst v) = show v
  cache (INeg expr) = "-(" ++ cache expr ++ ")"
  cache (ISum e1 e2) = "(" ++ cache e1 ++ ")+(" ++ cache e2 ++ ")"
  cache (IProd e1 e2) = "(" ++ cache e1 ++ ")*(" ++ cache e2 ++ ")"
  cache (IDiv e1 e2) = "(" ++ cache e1 ++ ")/(" ++ cache e2 ++ ")"
  cache (IMod e1 e2) = "(" ++ cache e1 ++ ")%(" ++ cache e2 ++ ")"
  cache (IPow e1 e2) = "(" ++ cache e1 ++ ")^(" ++ cache e2 ++ ")"
  cache (IVar name) = name

instance Cache REX where
  cache (RConst num) = cacheRN num
  cache (RNeg expr) = "-(" ++ cache expr ++ ")"
  cache (RInv expr) = "1/(" ++ cache expr ++ ")"
  cache (RSum e1 e2) = "(" ++ cache e1 ++ ")+(" ++ cache e2 ++ ")"
  cache (RProd e1 e2) = "(" ++ cache e1 ++ ")*(" ++ cache e2 ++ ")"
  cache (RDiv e1 e2) = "(" ++ cache e1 ++ ")/(" ++ cache e2 ++ ")"
  cache (RPow e1 e2) = "(" ++ cache e1 ++ ")^(" ++ cache e2 ++ ")"
  cache (RLog e1 e2) = "log_(" ++ cache e1 ++ ")(" ++ cache e2 ++ ")"
  cache (RVar name) = name
  cache (RApp func expr) = cacheBF func ++ "(" ++ cache expr ++ ")"

instance Cache BEX where
  cache (BConst True) = "1"
  cache (BConst False) = "0"
  cache (BVar name) = name
  cache (BNot expr) = "!(" ++ cache expr ++ ")"
  cache (BAnd e1 e2) = "(" ++ cache e1 ++ ")&(" ++ cache e2 ++ ")"
  cache (BOr e1 e2) = "(" ++ cache e1 ++ ")|(" ++ cache e2 ++ ")"
  cache (BXor e1 e2) = "(" ++ cache e1 ++ ")^(" ++ cache e2 ++ ")"
  cache (BImpl e1 e2) = "(" ++ cache e1 ++ ")=>(" ++ cache e2 ++ ")"
  cache (BEquiv e1 e2) = "(" ++ cache e1 ++ ")<=>(" ++ cache e2 ++ ")"
