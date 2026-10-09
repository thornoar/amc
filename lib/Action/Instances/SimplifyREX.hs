module Action.Instances.SimplifyREX (simplifyResult) where
import Object.Bundle
import Object.RealNumber
import Result

simplifyResult :: Object REX -> Result (Object REX)
simplifyResult obj = preEval obj >>= (preEval . normalize)

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

normalize :: Object REX -> Object REX
normalize (RConst v) = RConst v
normalize (RNeg obj) = RNeg (normalize obj)
normalize (RSum o1 o2) = RSum (normalize o1) (normalize o2)
normalize (RDiff o1 o2) = RDiff (normalize o1) (normalize o2)
normalize (RInv obj) = RInv (normalize obj)
normalize (RProd o1 o2) =
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (RVar n1, RVar n2)
    | n1 == n2 -> RPow o1' (RConst (In 2))
    | n1 < n2 -> RProd o1' o2'
    | otherwise -> RProd o2' o1'
  (RVar n1, RPow (RVar n2) o3)
    | n1 == n2 -> RPow (RVar n1) (RSum (RConst (In 1)) o3)
    | n1 < n2 -> RProd o1' o2'
    | otherwise -> RProd o2' o1'
  (RPow (RVar n1) o3, RVar n2)
    | n1 == n2 -> RPow (RVar n1) (RSum (RConst (In 1)) o3)
    | n1 < n2 -> RProd o1' o2'
    | otherwise -> RProd o2' o1'
  (RPow (RVar n1) o3, RPow (RVar n2) o4)
    | n1 == n2 -> RPow (RVar n1) (RSum o3 o4)
    | n1 < n2 -> RProd o1' o2'
    | otherwise -> RProd o2' o1'
  (RConst c1, RConst c2)
    | not (isNumericRN c1) && c1 == c2 -> RPow o1' (RConst (In 2))
  (RConst c1, RPow (RConst c2) o3)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RSum (RConst (In 1)) o3)
  (RPow (RConst c1) o3, RConst c2)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RSum (RConst (In 1)) o3)
  (RPow (RConst c1) o3, RPow (RConst c2) o4)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RSum o3 o4)
  (_, RProd (RConst v2) o3) -> RProd (RConst v2) (normalize (RProd o1' o3))
  (RProd (RConst v1) o3, _) -> RProd (RConst v1) (normalize (RProd o3 o2'))
  (_, RProd o3 o4) -> RProd (normalize (RProd o1' o3)) o4
  (RProd o3 o4, _) -> RProd (normalize (RProd o3 o2')) o4
  (RConst _, _) -> RProd o1' o2'
  (_, RPow _ _) -> RProd o2' o1'
  (_, RVar _) -> RProd o2' o1'
  _ -> RProd o1' o2'
normalize (RDiv o1 o2) =
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (RVar n1, RVar n2)
    | n1 == n2 -> RConst (In 1)
  (RVar n1, RPow (RVar n2) o3)
    | n1 == n2 -> RPow (RVar n1) (RDiff (RConst (In 1)) o3)
  (RPow (RVar n1) o3, RVar n2)
    | n1 == n2 -> RPow (RVar n1) (RDiff o3 (RConst (In 1)))
  (RPow (RVar n1) o3, RPow (RVar n2) o4)
    | n1 == n2 -> RPow (RVar n1) (RDiff o3 o4)
  (RConst c1, RConst c2)
    | not (isNumericRN c1) && c1 == c2 -> RConst (In 1)
  (RConst c1, RPow (RConst c2) o3)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RDiff (RConst (In 1)) o3)
  (RPow (RConst c1) o3, RConst c2)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RDiff o3 (RConst (In 1)))
  (RPow (RConst c1) o3, RPow (RConst c2) o4)
    | not (isNumericRN c1) && c1 == c2 -> RPow (RConst c1) (RDiff o3 o4)
  _ -> RDiv o1' o2'
normalize (RLog o1 o2) = RLog (normalize o1) (normalize o2)
normalize (RPow o1 o2) =
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (RPow o3 o4, _) -> RPow o3 (RProd o4 o2')
  _ -> RPow o1' o2'
normalize (RVar name) = RVar name
normalize (RApp fun obj) = RApp fun (normalize obj)

preEval :: Object REX -> Result (Object REX)
preEval (RConst v) = Content (RConst v)
preEval (RNeg (RNeg e)) = preEval e
preEval (RNeg (RSum e1 e2)) = preEval (RSum (RNeg e1) (RNeg e2))
preEval (RNeg expr) = preEval expr >>= \expr' -> Content $ case expr' of
  RConst v | isNumericRN v -> RConst (negRN v)
  expr'' -> RNeg expr''
preEval (RSum e1 e2) = do
  e1' <- preEval e1
  e2' <- preEval e2
  case (e1', e2') of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> Content (RConst (addRN c1 c2))
    (RConst c1, RSum (RConst c2) f2)
      | isNumericRN c1 && isNumericRN c2 -> Content (RSum (RConst (addRN c1 c2)) f2)
    (RConst c1, RSum f1 f2)
      | isNumericRN c1 -> preEval (RSum f1 (RSum (RConst c1) f2))
    (RSum (RConst c1) f2, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> Content (RSum (RConst (addRN c1 c2)) f2)
    (RSum f1 f2, RConst c2)
      | isNumericRN c2 -> preEval (RSum f1 (RSum f2 (RConst c2)))
    (e1'', RNeg e2'') -> Content (RDiff e1'' e2'')
    (RNeg e1'', e2'') -> Content (RDiff e2'' e1'')
    (e1'', e2'')
      | e1'' == e2'' -> Content (RProd (RConst (In 2)) e1'')
    (e1'', e2'') -> Content (RSum e1'' e2'')
preEval (RDiff e1 e2) = preEval (RSum e1 (RNeg e2))
preEval (RInv e) = preEval (RDiv (RConst (In 1)) e)
preEval (RProd e1 e2) = do
  e1' <- preEval e1
  e2' <- preEval e2
  case (e1', e2') of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> Content (RConst (mulRN c1 c2))
    (e1'', e2'') -> Content (RProd e1'' e2'')
preEval (RDiv e1 e2) = do
  e1' <- preEval e1
  e2' <- preEval e2
  case (e1', e2') of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> divRN c1 c2
    (e1'', e2'') -> Content (RDiv e1'' e2'')
preEval (RLog e1 e2) = do
  e1' <- preEval e1
  e2' <- preEval e2
  case (e1', e2') of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> logRN c1 c2
    (e1'', e2'') -> Content (RLog e1'' e2'')
preEval (RPow e1 e2) = do
  e1' <- preEval e1
  e2' <- preEval e2
  case (e1', e2') of
    (RConst c1, RConst c2)
      | isNumericRN c1 && isNumericRN c2 -> RConst <$> powRN c1 c2
    (e1'', e2'') -> Content (RPow e1'' e2'')
preEval (RVar name) = Content (RVar name)
preEval (RApp fun e) = preEval e >>= \e' -> case e' of
  RConst c | isNumericRN c -> RConst <$> applyRN fun c
  e'' -> Content (RApp fun e'')
