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

preEval :: Object IEX -> Object IEX
preEval (IConst v) = IConst v
preEval (IVar name) = IVar name
preEval (INeg (INeg e)) = preEval e
preEval (INeg (ISum e1 e2)) = preEval (ISum (INeg e1) (INeg e2))
preEval (INeg e) = INeg (preEval e)
preEval (ISum e1 e2) = undefined
preEval _ = undefined

-- preEval :: Object IEX -> Object IEX
-- preEval (IConst v) = IConst v
-- preEval (IVar name) = IVar name
-- preEval (INeg (INeg e)) = preEval e
-- preEval (INeg (ISum e1 e2)) = preEval (ISum (INeg e1) (INeg e2))
-- preEval (INeg e) = case preEval e of
--   IConst v -> IConst (negate v)
--   IProd (IConst c) r -> IProd (IConst (negate c)) r
--   e' -> INeg e'
-- preEval (ISum e1 e2) = mergeTerm (preEval e1) (preEval e2)
-- preEval (IProd e1 e2) = mergeFactor (preEval e1) (preEval e2)
-- preEval (IDiv e1 e2) = case (preEval e1, preEval e2) of
--   (IConst v1, IConst v2) -> IConst (v1 `div` v2)
--   (e1', e2') -> IDiv e1' e2'
-- preEval (IMod e1 e2) = case (preEval e1, preEval e2) of
--   (IConst v1, IConst v2) -> IConst (v1 `mod` v2)
--   (e1', e2') -> IMod e1' e2'
-- preEval (IPow e1 e2) = case (preEval e1, preEval e2) of
--   (IConst v1, IConst v2) -> IConst (v1 ^ v2)
--   (IPow b e, e2') -> preEval (IPow b (preEval (IProd e e2')))
--   (e1', e2') -> IPow e1' e2'
--
-- | Merge an evaluated summand into an evaluated (right-associated) sum.
--
-- The summands arrive in canonical order, so like terms are adjacent: if the
-- head combines with the next summand we keep going, otherwise it stays put.
-- mergeTerm :: Object IEX -> Object IEX -> Object IEX
-- mergeTerm h t
--   | h == IConst 0 = t
--   | t == IConst 0 = h
--   | otherwise = case h of
--       ISum h1 h2 -> mergeTerm h1 (mergeTerm h2 t)
--       _ -> case t of
--         ISum f rest -> maybe (ISum h t) (\c -> mergeTerm c rest) (combineLike h f)
--         _ -> maybe (ISum h t) id (combineLike h t)
--
-- -- | Merge an evaluated factor into an evaluated (right-associated) product.
-- mergeFactor :: Object IEX -> Object IEX -> Object IEX
-- mergeFactor h t
--   | h == IConst 0 = IConst 0
--   | h == IConst 1 = t
--   | t == IConst 1 = h
--   | otherwise = case h of
--       IProd h1 h2 -> mergeFactor h1 (mergeFactor h2 t)
--       _ -> case t of
--         IProd f rest -> maybe (IProd h t) (\c -> mergeFactor c rest) (combineFactors h f)
--         _ -> maybe (IProd h t) id (combineFactors h t)
--
-- -- | Combine two like summands (sharing a base) by adding their coefficients.
-- combineLike :: Object IEX -> Object IEX -> Maybe (Object IEX)
-- combineLike t1 t2 =
--   let (c1, b1) = splitCoeff t1
--       (c2, b2) = splitCoeff t2
--    in if b1 == b2 then Just (buildCoeffTerm b1 (c1 + c2)) else Nothing
--
-- -- | Combine two factors: multiply constants, or add exponents of equal bases.
-- combineFactors :: Object IEX -> Object IEX -> Maybe (Object IEX)
-- combineFactors a b = case (a, b) of
--   (IConst ca, IConst cb) -> Just (IConst (ca * cb))
--   _ ->
--     let (ba, ea) = splitPow a
--         (bb, eb) = splitPow b
--      in if ba == bb then Just (buildPow ba (preEval (ISum ea eb))) else Nothing

-- | Split a summand into its integer coefficient and its remaining base.
splitCoeff :: Object IEX -> (Integer, Object IEX)
splitCoeff (IConst c) = (c, IConst 1)
splitCoeff (INeg e) = let (c, b) = splitCoeff e in (negate c, b)
splitCoeff (IProd e1 e2) =
  let (c1, b1) = splitCoeff e1
      (c2, b2) = splitCoeff e2
   in (c1 * c2, productOf [b1, b2])
splitCoeff e = (1, e)

-- buildCoeffTerm :: Object IEX -> Integer -> Object IEX
-- buildCoeffTerm _ 0 = IConst 0
-- buildCoeffTerm (IConst 1) c = IConst c
-- buildCoeffTerm b 1 = b
-- buildCoeffTerm b c = IProd (IConst c) b

-- | Build a right-associated product, dropping multiplicative identity @1@.
productOf :: [Object IEX] -> Object IEX
productOf es = case filter (/= IConst 1) es of
  [] -> IConst 1
  fs -> foldr1 IProd fs

-- -- | Split a factor into its base and exponent (a bare factor has exponent 1).
-- splitPow :: Object IEX -> (Object IEX, Object IEX)
-- splitPow (IPow b e) = (b, e)
-- splitPow e = (e, IConst 1)
--
-- buildPow :: Object IEX -> Object IEX -> Object IEX
-- buildPow _ (IConst 0) = IConst 1
-- buildPow b (IConst 1) = b
-- buildPow b e = IPow b e
