module Action.Instances.SimplifyIEX (simplify) where
import Object.Bundle
import Data.List (sortBy)
import Data.Ord (comparing)

simplify :: Object IEX -> Object IEX
simplify = preEval . rearrange

-- | Put an integer expression into a canonical order.
--
-- Sums and products are right-associated, and the operands of every sum and
-- product are sorted by 'operandKey': constant terms first, then powers of
-- variables in alphabetical order (a bare variable comes before the same
-- variable raised to a power), then all remaining terms in their original
-- order.
rearrange :: Object IEX -> Object IEX
rearrange (IConst v) = IConst v
rearrange (IVar name) = IVar name
rearrange (INeg e) = INeg (rearrange e)
rearrange (ISum e1 e2) = foldr1 ISum $ sortBy (comparing operandKey) $ summands (ISum e1 e2)
rearrange (IProd e1 e2) = foldr1 IProd $ sortBy (comparing operandKey) $ factors (IProd e1 e2)
rearrange (IDiv e1 e2) = IDiv (rearrange e1) (rearrange e2)
rearrange (IMod e1 e2) = IMod (rearrange e1) (rearrange e2)
rearrange (IPow e1 e2) = IPow (rearrange e1) (rearrange e2)

-- | Flatten the top-level sum spine and rearrange every summand.
summands :: Object IEX -> [Object IEX]
summands (ISum e1 e2) = summands e1 ++ summands e2
summands e = [rearrange e]

-- | Flatten the top-level product spine and rearrange every factor.
factors :: Object IEX -> [Object IEX]
factors (IProd e1 e2) = factors e1 ++ factors e2
factors e = [rearrange e]

-- | Sort key placing constants first, then variable powers (by variable name,
-- then by exponent), then everything else. The coefficient of a term is
-- ignored, so that like terms (e.g. @x@ and @4*x@) end up next to each other.
operandKey :: Object IEX -> (Int, String, Integer)
operandKey e = case snd (splitCoeff e) of
  IConst _ -> (0, "", 0)
  IVar name -> (1, name, 1)
  IPow (IVar name) (IConst pow) -> (1, name, pow)
  _ -> (2, "", 0)

-- | Split a summand into its integer coefficient and its remaining base.
splitCoeff :: Object IEX -> (Integer, Object IEX)
splitCoeff (IConst c) = (c, IConst 1)
splitCoeff (INeg e) = let (c, b) = splitCoeff e in (negate c, b)
splitCoeff (IProd e1 e2) =
  let (c1, b1) = splitCoeff e1
      (c2, b2) = splitCoeff e2
   in (c1 * c2, productOf [b1, b2])
splitCoeff e = (1, e)

-- | Build a right-associated product, dropping multiplicative identity @1@.
productOf :: [Object IEX] -> Object IEX
productOf es = case filter (/= IConst 1) es of
  [] -> IConst 1
  fs -> foldr1 IProd fs

-- | Simplify an integer expression in canonical order
preEval :: Object IEX -> Object IEX
preEval (IConst v) = IConst v
preEval (IVar name) = IVar name
preEval (INeg (INeg e)) = preEval e
preEval (INeg (ISum e1 e2)) = preEval (ISum (INeg e1) (INeg e2))
preEval (INeg e) = case preEval e of
  IConst v -> IConst (negate v)
  IProd (IConst c) r -> IProd (IConst (negate c)) r
  e' -> INeg e'
preEval (ISum e1 e2) = case (preEval e1, preEval e2) of
  (f1, ISum f2 f3) -> case combineSum f1 f2 of
    ISum g1 g2 -> ISum g1 (ISum g2 f3)
    IConst 0 -> f3
    g1 -> ISum g1 f3
  (f1, f2) -> combineSum f1 f2
preEval (IProd e1 e2) = case (preEval e1, preEval e2) of
  (f1, IProd f2 f3) -> case combineProd f1 f2 of
    ISum g1 g2 -> IProd g1 (IProd g2 f3)
    g1 -> IProd g1 f3
  (f1, f2) -> combineProd f1 f2
preEval (IDiv e1 e2) = case (preEval e1, preEval e2) of
  (IConst c1, IConst c2) -> IConst (div c1 c2)
  (f1, f2) -> IDiv f1 f2
preEval (IMod e1 e2) = case (preEval e1, preEval e2) of
  (IConst c1, IConst c2) -> IConst (mod c1 c2)
  (f1, f2) -> IMod f1 f2
preEval (IPow e1 e2) = case (preEval e1, preEval e2) of
  (IConst c1, IConst c2) -> IConst (c1 ^ c2)
  (f1, f2) -> IPow f1 f2
preEval (IVar nme) = IVar nme
-- preEval _ = undefined

-- Take two summands and combine them as appropriate
combineSum :: Object IEX -> Object IEX -> Object IEX
combineSum (IConst 0) e = e
combineSum e (IConst 0) = e
combineSum (IConst c1) (IConst c2) = IConst $ c1 + c2
combineSum (IConst c) e = ISum (IConst c) e
combineSum (INeg e1) (INeg e2) = case (e1, e2) of
  (_, IProd f1 f2)
    | e1 == f2 -> INeg $ IProd (preEval $ ISum (IConst 1) f1) f2
  (IProd f1 f2, _)
    | f1 == e2 -> INeg $ IProd (preEval $ ISum (IConst 1) f1) f2
  _
    | e1 == e2 -> INeg $ IProd (IConst 2) e1
  _ -> ISum (INeg e1) (INeg e2)
combineSum (INeg e1) e2 = case (e1, e2) of
  (_, IProd f1 f2)
    | e1 == f2 -> IProd (preEval $ ISum (IConst (-1)) f1) f2
  (IProd f1 f2, _)
    | f1 == e2 -> IProd (preEval $ ISum (IConst 1) (INeg f1)) f2
  _
    | e1 == e2 -> IConst 0
  _ -> ISum (INeg e1) e2
combineSum e1 (INeg e2) = case (e1, e2) of
  (_, IProd f1 f2)
    | e1 == f2 -> IProd (preEval $ ISum (IConst 1) (INeg f1)) f2
  (IProd f1 f2, _)
    | f1 == e2 -> IProd (preEval $ ISum (IConst (-1)) f1) f2
  _
    | e1 == e2 -> IConst 0
  _ -> ISum e1 (INeg e2)
combineSum e1 e2 = case (e1, e2) of
  (_, IProd f1 f2)
    | e1 == f2 -> IProd (preEval $ ISum (IConst 1) f1) f2
  (IProd f1 f2, _)
    | f1 == e2 -> IProd (preEval $ ISum (IConst 1) f1) f2
  _
    | e1 == e2 -> IProd (IConst 2) e1
  _ -> ISum e1 e2

-- | Split a factor into its base and exponent (a bare factor has exponent 1).
splitPow :: Object IEX -> (Object IEX, Object IEX)
splitPow (IPow b e) = (b, e)
splitPow e = (e, IConst 1)

-- | Build a power, dropping the trivial exponents 0 and 1.
buildPow :: Object IEX -> Object IEX -> Object IEX
buildPow _ (IConst 0) = IConst 1
buildPow b (IConst 1) = b
buildPow b e = IPow b e

-- | Take two multiplicands and combine them as appropriate
combineProd :: Object IEX -> Object IEX -> Object IEX
combineProd (IConst 0) _ = IConst 0
combineProd _ (IConst 0) = IConst 0
combineProd (IConst 1) e = e
combineProd e (IConst 1) = e
combineProd (IConst c1) (IConst c2) = IConst $ c1 * c2
combineProd (INeg e1) (INeg e2) = combineProd e1 e2
combineProd (INeg e1) e2 = INeg $ combineProd e1 e2
combineProd e1 (INeg e2) = INeg $ combineProd e1 e2
combineProd e1 e2 = case (splitPow e1, splitPow e2) of
  ((b1, p1), (b2, p2))
    | b1 == b2 -> buildPow b1 (preEval $ ISum p1 p2)
  _ -> IProd e1 e2
