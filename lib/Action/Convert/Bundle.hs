{-# LANGUAGE UndecidableInstances #-}
module Action.Convert.Bundle (ConvertResult, convertResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Input (IIO, getColoredInputLine, promptLength, splitPair)
import Result
import Description
import Object.RealNumber (RealNumber(Dbl))

data Unit =
    NanoM | MicroM | MilliM | CentiM | DeciM | Meter | KiloM | Mile | Inch | Foot
  | MilliL
  deriving (Eq, Read, Show)

displayUnit :: Unit -> String
displayUnit NanoM = "nanometers"
displayUnit _ = undefined

convertFactor :: (Unit, Unit) -> Result Double
convertFactor (u, u')
  | u == u' = Content 1.0
convertFactor (NanoM, u) = (/10000000.0) <$> convertFactor (CentiM, u)
convertFactor (MicroM, u) = (/1000.0) <$> convertFactor (CentiM, u)
convertFactor (MilliM, u) = (/10.0) <$> convertFactor (CentiM, u)
convertFactor (CentiM, u) = convertFactor (u, CentiM)
convertFactor (DeciM, u) = (*10.0) <$> convertFactor (CentiM, u)
convertFactor (Meter, u) = (*100.0) <$> convertFactor (CentiM, u)
convertFactor (KiloM, u) = (*100000.0) <$> convertFactor (CentiM, u)
convertFactor (Mile, u) = (*160934.4) <$> convertFactor (CentiM, u)
convertFactor (Inch, u) = (*2.54) <$> convertFactor (CentiM, u)
convertFactor (Foot, u) = (*30.48) <$> convertFactor (CentiM, u)
convertFactor (u1, u2) = Error $ displayUnit u1 ++ " cannot be converted to " ++ displayUnit u2

guessUnit :: String -> Result Unit
guessUnit "nm" = Content NanoM
guessUnit "nanometers" = Content NanoM
guessUnit str = Error $ "could not recognize unit: `" ++ str ++ "`"

getUnits :: IIO (Result (Unit, Unit))
getUnits = do
  let prstr = "from > to: "
      go (ustr1, ustr2) = (,) <$> guessUnit ustr1 <*> guessUnit ustr2
  input <- getColoredInputLine $ replicate (promptLength - length prstr) '#' ++ prstr
  return $ input >>= go . splitPair (== '>') . filter (/= ' ')

type ConvertResult :: ObjectTag -> Constraint
class ConvertResult tg where
  convertResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => ConvertResult tg where
  convertResult obj = return . Error $ (description $ proxyOf obj) ++ " cannot be used for unit conversion"

instance {-# OVERLAPPING #-} ConvertResult IEX where
  convertResult obj = getUnits >>= return . (fmap $ IProd obj . IConst . floor) . (>>= convertFactor)

instance {-# OVERLAPPING #-} ConvertResult REX where
  convertResult obj = getUnits >>= return . (fmap $ RProd obj . RConst . Dbl) . (>>= convertFactor)
