module Action.Instances.SimplifyIEX (simplify) where
import Object.Bundle

simplify :: Object IEX -> Object IEX
simplify = preEval . normalize . preEval

-- collectTerms :: Object IEX -> [(String, Integer)]
-- collectTerms = undefined
--
-- convertTerm :: [(String, Integer)] -> Object IEX
-- convertTerm [] = IConst 1
-- convertTerm ((name,pow):rest) = IProd (IPow (IVar name) (IConst pow)) $ convertTerm rest

normalize :: Object IEX -> Object IEX
normalize (IConst v) = IConst v
normalize (INeg obj) = INeg (normalize obj)
normalize (ISum o1 o2) = ISum (normalize o1) (normalize o2)
normalize (IDiff o1 o2) = IDiff (normalize o1) (normalize o2)
normalize (IProd o1 o2) =
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (IVar n1, IVar n2)
    | n1 == n2 -> IPow o1' (IConst 2)
    | n1 < n2 -> IProd o1' o2'
    | otherwise -> IProd o2' o1'
  (IVar n1, IPow (IVar n2) o3)
    | n1 == n2 -> IPow (IVar n1) (ISum (IConst 1) o3)
    | n1 < n2 -> IProd o1' o2'
    | otherwise -> IProd o2' o1'
  (IPow (IVar n1) o3, IVar n2)
    | n1 == n2 -> IPow (IVar n1) (ISum (IConst 1) o3)
    | n1 < n2 -> IProd o1' o2'
    | otherwise -> IProd o2' o1'
  (IPow (IVar n1) o3, IPow (IVar n2) o4)
    | n1 == n2 -> IPow (IVar n1) (ISum o3 o4)
    | n1 < n2 -> IProd o1' o2'
    | otherwise -> IProd o2' o1'
  (_, IProd (IConst v2) o3) -> IProd (IConst v2) (normalize (IProd o1' o3))
  (IProd (IConst v1) o3, _) -> IProd (IConst v1) (normalize (IProd o3 o2'))
  (_, IProd o3 o4) -> IProd (normalize (IProd o1' o3)) o4
  (IProd o3 o4, _) -> IProd (normalize (IProd o3 o2')) o4
  (IConst _, _) -> IProd o1' o2'
  (_, (IPow _ _)) -> IProd o2' o1'
  (_, IVar _) -> IProd o2' o1'
  _ -> IProd o1' o2'
normalize (IDiv o1 o2) = 
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (IVar n1, IVar n2)
    | n1 == n2 -> IConst 1
  (IVar n1, IPow (IVar n2) o3)
    | n1 == n2 -> IPow (IVar n1) (IDiff (IConst 1) o3)
  (IPow (IVar n1) o3, IVar n2)
    | n1 == n2 -> IPow (IVar n1) (IDiff o3 (IConst 1))
  (IPow (IVar n1) o3, IPow (IVar n2) o4)
    | n1 == n2 -> IPow (IVar n1) (IDiff o3 o4)
  _ -> IDiv o1' o2'
normalize (IMod o1 o2) = IMod (normalize o1) (normalize o2)
normalize (IPow o1 o2) =
  let (o1', o2') = (normalize o1, normalize o2) in case (o1', o2') of
  (IPow o3 o4, _) -> IPow o3 (IProd o4 o2')
  _ -> IPow o1' o2'
normalize (IVar name) = IVar name

preEval :: Object IEX -> Object IEX
preEval (IConst v) = IConst v
preEval (INeg (INeg e)) = preEval e
preEval (INeg (ISum e1 e2)) = preEval (ISum (INeg e1) (INeg e2))
preEval (INeg expr) = case preEval expr of
  IConst v -> IConst (-v)
  expr' -> INeg expr'
preEval (ISum e1 e2) = case (preEval e1, preEval e2) of
  (IConst c1, IConst c2) -> IConst (c1 + c2)
  (IConst c1, ISum (IConst c2) f2) -> ISum (IConst (c1 + c2)) f2
  (IConst c1, ISum f1 f2) -> ISum f1 (preEval $ ISum (IConst c1) f2)
  (ISum (IConst c1) f2, IConst c2) -> ISum (IConst (c1 + c2)) f2
  (ISum f1 f2, IConst c2) -> ISum f1 (preEval $ ISum f2 (IConst c2))
  (e1', INeg e2') -> IDiff e1' e2'
  (INeg e1', e2') -> IDiff e2' e1'
  (e1', e2') -> ISum e1' e2'
preEval (IDiff e1 e2) = preEval (ISum e1 (INeg e2))
preEval (IProd e1 e2) = case (preEval e1, preEval e2) of
  (IConst c1, IConst c2) -> IConst (c1 * c2)
  (e1', e2') -> IProd e1' e2'
preEval (IDiv e1 e2) = case (preEval e1, preEval e2) of
  (IConst v1, IConst v2) -> IConst (v1 `div` v2)
  (e1', e2') -> IDiv e1' e2'
preEval (IMod e1 e2) = case (preEval e1, preEval e2) of
  (IConst v1, IConst v2) -> IConst (v1 `mod` v2)
  (e1', e2') -> IMod e1' e2'
preEval (IPow e1 e2) = case (preEval e1, preEval e2) of
  (IConst v1, IConst v2) -> IConst (v1 ^ v2)
  (e1', e2') -> IPow e1' e2'
preEval (IVar name) = IVar name
