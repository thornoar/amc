{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE InstanceSigs #-}

module Object.RealNumber where
-- import Data.Ratio ((%))

data RealNumber = In Integer | Dbl Double | PI | E | GAMMA | LN2
  deriving (Read, Show)

data BuiltinFunction = Exp | Ln | Sin | Cos | Tan | Cot
  deriving (Show, Read)

constToDouble :: RealNumber -> Double
constToDouble (In value) = fromIntegral value
-- constToDouble (Rt value) = fromRational value
constToDouble (Dbl value) = value
constToDouble PI = 3.141592653589793238462643383279502884
constToDouble E = 2.718281828459045235360287471352662498
constToDouble GAMMA = 0.577215664901532860606512090082402431
constToDouble LN2 = 0.693147180559945309417232121458176568

-- isPositiveConst :: RealConst -> Bool
-- isPositiveConst (Rt r) = r > 0
-- isPositiveConst (Dbl v) = v > 0
-- isPositiveConst _ = True
--
-- data RealNumber =
--     Const RealConst
--   | Neg RealNumber
--   -- | Inv RealNumber
--   | Sum RealNumber RealNumber
--   | Diff RealNumber RealNumber
--   | Prod RealNumber RealNumber
--   | Div RealNumber RealNumber
--   | Exp RealNumber
--   | Ln RealNumber
--   | Root Integer RealNumber
--   | Pow RealNumber Integer
--   deriving (Read, Show)
--
-- instance Num RealNumber where
--   (+) :: RealNumber -> RealNumber -> RealNumber
--   (+) = Sum
--   (*) :: RealNumber -> RealNumber -> RealNumber
--   (*) = Prod
--   abs :: RealNumber -> RealNumber
--   abs v = if isPositive v then v else (Neg v)
--   signum :: RealNumber -> RealNumber
--   signum v = Const . Rt . (% 1) . floor . signum $ (toDouble v)
--   fromInteger :: Integer -> RealNumber
--   fromInteger = Const . Rt . (% 1)
--   negate :: RealNumber -> RealNumber
--   negate = Neg
--
-- toDouble :: RealNumber -> Double
-- toDouble (Const v) = constToDouble v
-- toDouble (Neg v) = negate (toDouble v)
-- -- toDouble (Inv v) = recip (toDouble v)
-- toDouble (Sum left right) = toDouble left + toDouble right
-- toDouble (Diff left right) = toDouble left - toDouble right
-- toDouble (Prod left right) = toDouble left * toDouble right
-- toDouble (Div left right) = toDouble left / toDouble right
-- toDouble (Exp v) = exp (toDouble v)
-- toDouble (Ln v) = log (toDouble v)
-- toDouble (Root n v) = (toDouble v) ** (recip (fromIntegral n))
-- toDouble (Pow v n) = (toDouble v) ^^ n
--
-- isPositive :: RealNumber -> Bool
-- isPositive = (> 0) . toDouble
