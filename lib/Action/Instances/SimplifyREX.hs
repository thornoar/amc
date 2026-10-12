module Action.Instances.SimplifyREX (simplifyResult) where
import Object.Bundle
import Object.RealNumber
import Result
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Simplify a real expression in canonical order.
--
-- Numeric constants (@In@ and @Dbl@) are evaluated as usual, but the
-- symbolic constants @PI@, @E@, @GAMMA@ and @LN2@ are treated like variables,
-- so that e.g. @pi + pi@ becomes @2*pi@ rather than a decimal approximation.
simplifyResult :: Object REX -> Result (Object REX)
simplifyResult obj = fmap finalize (preEval (rearrange (expandDiffs obj)))

addRN :: RealNumber -> RealNumber -> RealNumber
addRN (In a) (In b) = In (a + b)
addRN a b = Dbl (constToDouble a + constToDouble b)

mulRN :: RealNumber -> RealNumber -> RealNumber
mulRN (In a) (In b) = In (a * b)
mulRN a b = Dbl (constToDouble a * constToDouble b)

negRN :: RealNumber -> RealNumber
negRN (In a) = In (negate a)
negRN a = Dbl (negate (constToDouble a))

isZeroRN :: RealNumber -> Bool
isZeroRN (In 0) = True
isZeroRN (Dbl 0) = True
isZeroRN _ = False

isOneRN :: RealNumber -> Bool
isOneRN (In 1) = True
isOneRN (Dbl 1) = True
isOneRN _ = False

isMinusOneRN :: RealNumber -> Bool
isMinusOneRN (In (-1)) = True
isMinusOneRN (Dbl (-1)) = True
isMinusOneRN _ = False

isNumericRN :: RealNumber -> Bool
isNumericRN (In _) = True
isNumericRN (Dbl _) = True
isNumericRN _ = False

divRN :: RealNumber -> RealNumber -> Result RealNumber
divRN _ d | isZeroRN d = Error "division by zero"
divRN (In a) (In b)
  | a `mod` b == 0 = Content (In (a `div` b))
divRN a b = Content (Dbl (constToDouble a / constToDouble b))

powRN :: RealNumber -> RealNumber -> Result RealNumber
powRN _ e | isZeroRN e = Content (In 1)
powRN (In x) (In n)
  | n > 0 = Content (In (x ^ n))
  | x == 1 = Content (In 1)
  | x == -1 = Content (In (if even n then 1 else -1))
  | x == 0 = Error "division by zero"
  | otherwise = Content (Dbl (fromIntegral x ** fromIntegral n))
powRN b e =
  let v = constToDouble b ** constToDouble e
   in if isNaN v then Error "result is not a real number" else Content (Dbl v)

logRN :: RealNumber -> RealNumber -> Result RealNumber
logRN base x
  | constToDouble base <= 0 = Error "logarithm base must be positive"
  | constToDouble base == 1 = Error "logarithm base must not be one"
  | constToDouble x <= 0 = Error "logarithm argument must be positive"
  | otherwise = Content (Dbl (logBase (constToDouble base) (constToDouble x)))

applyRN :: BuiltinFunction -> RealNumber -> Result RealNumber
applyRN Exp v = Content (Dbl (exp (constToDouble v)))
applyRN Ln v
  | constToDouble v <= 0 = Error "logarithm argument must be positive"
  | otherwise = Content (Dbl (log (constToDouble v)))
applyRN Sin v = Content (Dbl (sin (constToDouble v)))
applyRN Cos v = Content (Dbl (cos (constToDouble v)))
applyRN Tan v = Content (Dbl (tan (constToDouble v)))
applyRN Cot v
  | s == 0 = Error "cotangent is undefined"
  | otherwise = Content (Dbl (cos (constToDouble v) / s))
  where s = sin (constToDouble v)

isZeroObj :: Object REX -> Bool
isZeroObj (RConst c) = isZeroRN c
isZeroObj _ = False

isOneObj :: Object REX -> Bool
isOneObj (RConst c) = isOneRN c
isOneObj _ = False

-- | Rewrite every subtraction as a sum with a negated right operand, so that
-- the rest of the pipeline only has to deal with sums and products.
expandDiffs :: Object REX -> Object REX
expandDiffs (RDiff e1 e2) = RSum (expandDiffs e1) (RNeg (expandDiffs e2))
expandDiffs (RSum e1 e2) = RSum (expandDiffs e1) (expandDiffs e2)
expandDiffs (RNeg e) = RNeg (expandDiffs e)
expandDiffs (RProd e1 e2) = RProd (expandDiffs e1) (expandDiffs e2)
expandDiffs (RDiv e1 e2) = RDiv (expandDiffs e1) (expandDiffs e2)
expandDiffs (RInv e) = RInv (expandDiffs e)
expandDiffs (RLog e1 e2) = RLog (expandDiffs e1) (expandDiffs e2)
expandDiffs (RPow e1 e2) = RPow (expandDiffs e1) (expandDiffs e2)
expandDiffs (RApp f e) = RApp f (expandDiffs e)
expandDiffs e = e

-- | Put an expression into a canonical order: sums and products are
-- right-associated and their operands are sorted by 'operandKey'.
rearrange :: Object REX -> Object REX
rearrange (RConst v) = RConst v
rearrange (RVar name) = RVar name
rearrange (RNeg e) = RNeg (rearrange e)
rearrange (RSum e1 e2) = foldr1 RSum $ sortBy (comparing operandKey) $ summands (RSum e1 e2)
rearrange (RDiff e1 e2) = rearrange (RSum e1 (RNeg e2))
rearrange (RProd e1 e2) = foldr1 RProd $ sortBy (comparing operandKey) $ factors (RProd e1 e2)
rearrange (RDiv e1 e2) = RDiv (rearrange e1) (rearrange e2)
rearrange (RInv e) = RInv (rearrange e)
rearrange (RLog e1 e2) = RLog (rearrange e1) (rearrange e2)
rearrange (RPow e1 e2) = RPow (rearrange e1) (rearrange e2)
rearrange (RApp f e) = RApp f (rearrange e)

summands :: Object REX -> [Object REX]
summands (RSum e1 e2) = summands e1 ++ summands e2
summands (RDiff e1 e2) = summands e1 ++ map RNeg (summands e2)
summands e = [rearrange e]

factors :: Object REX -> [Object REX]
factors (RProd e1 e2) = factors e1 ++ factors e2
factors e = [rearrange e]

-- | Sort key placing numeric constants first, then variables and symbolic
-- constants (by name, then by exponent), then everything else.
operandKey :: Object REX -> (Int, String, Double)
operandKey e = case snd (splitCoeff e) of
  RConst c | isNumericRN c -> (0, "", 0)
  RVar name -> (1, name, 1)
  RConst c -> (1, constName c, 1)
  RPow (RVar name) (RConst p) -> (1, name, constToDouble p)
  RPow (RConst c) (RConst p)
    | not (isNumericRN c) -> (1, constName c, constToDouble p)
  _ -> (2, "", 0)

constName :: RealNumber -> String
constName PI = "pi"
constName E = "e"
constName GAMMA = "gamma"
constName LN2 = "ln2"
constName _ = ""

-- | Split a summand into its numeric coefficient and its remaining base.
splitCoeff :: Object REX -> (RealNumber, Object REX)
splitCoeff (RConst c) | isNumericRN c = (c, RConst (In 1))
splitCoeff (RNeg e) = let (c, b) = splitCoeff e in (negRN c, b)
splitCoeff (RProd e1 e2) =
  let (c1, b1) = splitCoeff e1
      (c2, b2) = splitCoeff e2
   in (mulRN c1 c2, productOf [b1, b2])
splitCoeff e = (In 1, e)

-- | Build a right-associated product, dropping multiplicative identity @1@.
productOf :: [Object REX] -> Object REX
productOf es = case filter (not . isOneObj) es of
  [] -> RConst (In 1)
  fs -> foldr1 RProd fs

-- | Build a coefficient times a base, dropping trivial cases.
buildCoeffTerm :: Object REX -> RealNumber -> Object REX
buildCoeffTerm _ c | isZeroRN c = RConst (In 0)
buildCoeffTerm b c | isOneRN c = b
buildCoeffTerm b c | isMinusOneRN c = negateProd b
buildCoeffTerm b c | isOneObj b = RConst c
buildCoeffTerm b c = RProd (RConst c) b

-- | Simplify an expression in canonical order.
preEval :: Object REX -> Result (Object REX)
preEval (RConst v) = Content (RConst v)
preEval (RVar name) = Content (RVar name)
preEval (RApp fun e) = preEval e >>= \e' -> case e' of
  RConst c | isNumericRN c -> RConst <$> applyRN fun c
  _ -> Content (RApp fun e')
preEval (RNeg (RNeg e)) = preEval e
preEval (RNeg (RSum e1 e2)) = preEval (RSum (RNeg e1) (RNeg e2))
preEval (RNeg (RDiff e1 e2)) = preEval (RSum (RNeg e1) e2)
preEval (RNeg e) = preEval e >>= \e' -> Content $ case e' of
  RConst c | isNumericRN c -> RConst (negRN c)
  RProd (RConst c) r | isNumericRN c -> RProd (RConst (negRN c)) r
  e'' -> RNeg e''
preEval (RSum e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case f2 of
    RSum f2' f3 -> case combineSum f1 f2' of
      RSum g1 g2 -> Content (RSum g1 (RSum g2 f3))
      RConst c | isZeroRN c -> Content f3
      g1 -> Content (RSum g1 f3)
    _ -> Content (combineSum f1 f2)
preEval (RDiff e1 e2) = preEval (RSum e1 (RNeg e2))
preEval (RProd e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case f2 of
    RProd f2' f3 -> do
      g1 <- combineProd f1 f2'
      case g1 of
        RSum g2 g3 -> Content (RProd g2 (RProd g3 f3))
        g1' -> combineProd g1' f3
    _ -> combineProd f1 f2
preEval (RDiv e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case (f1, f2) of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> divRN c1 c2
    _ | isZeroObj f2 -> Error "division by zero"
    _ | f1 == f2 -> Content (RConst (In 1))
    _ -> case (splitPow f1, splitPow f2) of
      ((b1, p1), (b2, p2))
        | b1 == b2 -> preEval (RPow b1 (RSum p1 (RNeg p2)))
      _ -> Content (RDiv f1 f2)
preEval (RInv e) = preEval (RDiv (RConst (In 1)) e)
preEval (RLog e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case (f1, f2) of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> logRN c1 c2
    _ -> Content (RLog f1 f2)
preEval (RPow e1 e2) = do
  f1 <- preEval e1
  f2 <- preEval e2
  case (f1, f2) of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> powRN c1 c2
    (_, RConst c) | isZeroRN c -> Content (RConst (In 1))
    (_, RConst c) | isOneRN c -> Content f1
    (RPow b p, _) -> preEval (RPow b (RProd p f2))
    _ -> Content (RPow f1 f2)

-- | Take two summands and combine them as appropriate.
combineSum :: Object REX -> Object REX -> Object REX
combineSum e1 e2
  | isZeroObj e1 = e2
  | isZeroObj e2 = e1
  | b1 == b2 = buildCoeffTerm b1 (addRN c1 c2)
  | otherwise = RSum e1 e2
  where
    (c1, b1) = splitCoeff e1
    (c2, b2) = splitCoeff e2

-- | Split a factor into its base and exponent (a bare factor has exponent 1).
splitPow :: Object REX -> (Object REX, Object REX)
splitPow (RPow b e) = (b, e)
splitPow e = (e, RConst (In 1))

-- | Build a power, dropping the trivial exponents 0 and 1.
buildPow :: Object REX -> Object REX -> Object REX
buildPow _ e | isZeroObj e = RConst (In 1)
buildPow b e | isOneObj e = b
buildPow b e = RPow b e

-- | Negate a factor, folding the sign into a constant or coefficient.
negateProd :: Object REX -> Object REX
negateProd (RNeg e) = e
negateProd (RConst c) | isNumericRN c = RConst (negRN c)
negateProd (RProd (RConst c) r) | isNumericRN c = RProd (RConst (negRN c)) r
negateProd (RSum e1 e2) = RSum (negateProd e1) (negateProd e2)
negateProd e = RNeg e

-- | Take two multiplicands and combine them as appropriate.
combineProd :: Object REX -> Object REX -> Result (Object REX)
combineProd e1 e2
  | isZeroObj e1 || isZeroObj e2 = Content (RConst (In 0))
  | isOneObj e1 = Content e2
  | isOneObj e2 = Content e1
  | otherwise = case (e1, e2) of
      (RConst c1, RConst c2)
        | isNumericRN c1 && isNumericRN c2 -> Content (RConst (mulRN c1 c2))
      (RNeg a, RNeg b) -> combineProd a b
      (RNeg a, b) -> negateProd <$> combineProd a b
      (a, RNeg b) -> negateProd <$> combineProd a b
      (RConst c, b) | isMinusOneRN c -> Content (negateProd b)
      (a, RConst c) | isMinusOneRN c -> Content (negateProd a)
      _ -> case (splitPow e1, splitPow e2) of
        ((b1, p1), (b2, p2))
          | b1 == b2 -> buildPow b1 <$> preEval (RSum p1 p2)
        _ -> Content (RProd e1 e2)

-- | Render internal sums back as subtractions where possible.
finalize :: Object REX -> Object REX
finalize (RSum e1 e2) = rebuildSum (map finalize (flattenSum (RSum e1 e2)))
finalize (RDiff e1 e2) = finalize (RSum e1 (RNeg e2))
finalize (RNeg e) = case finalize e of
  RConst c | isNumericRN c -> RConst (negRN c)
  RProd (RConst c) r | isNumericRN c -> RProd (RConst (negRN c)) r
  e' -> RNeg e'
finalize (RProd e1 e2) = RProd (finalize e1) (finalize e2)
finalize (RDiv e1 e2) = RDiv (finalize e1) (finalize e2)
finalize (RInv e) = RInv (finalize e)
finalize (RLog e1 e2) = RLog (finalize e1) (finalize e2)
finalize (RPow e1 e2) = RPow (finalize e1) (finalize e2)
finalize (RApp f e) = RApp f (finalize e)
finalize e = e

flattenSum :: Object REX -> [Object REX]
flattenSum (RSum e1 e2) = flattenSum e1 ++ flattenSum e2
flattenSum e = [e]

rebuildSum :: [Object REX] -> Object REX
rebuildSum [] = RConst (In 0)
rebuildSum (t : ts) = foldl step t ts
  where
    step acc (RNeg t') = RDiff acc t'
    step acc (RConst c) | isNumericRN c && constToDouble c < 0 = RDiff acc (RConst (negRN c))
    step acc (RProd (RConst c) r)
      | isNumericRN c && constToDouble c < 0 = RDiff acc (RProd (RConst (negRN c)) r)
    step acc s = RSum acc s
