module Action.Instances.SimplifyIEX (simplify) where
import Object.Bundle

simplify :: Object IEX -> Object IEX
simplify (IConst v) = IConst v
simplify (INeg (INeg e)) = simplify e
simplify (INeg (ISum e1 e2)) = simplify (ISum (INeg e1) (INeg e2))
simplify (INeg expr) = case simplify expr of
  IConst v -> IConst (-v)
  expr' -> INeg expr'
simplify (ISum e1 e2) = case (simplify e1, simplify e2) of
  (IConst c1, IConst c2) -> IConst (c1 + c2)
  (IConst c1, ISum (IConst c2) f2) -> ISum (IConst (c1 + c2)) f2
  (IConst c1, ISum f1 f2) -> ISum f1 (simplify $ ISum (IConst c1) f2)
  (ISum (IConst c1) f2, IConst c2) -> ISum (IConst (c1 + c2)) f2
  (ISum f1 f2, IConst c2) -> ISum f1 (simplify $ ISum f2 (IConst c2))
  (e1', e2') -> ISum e1' e2'
simplify (IProd e1 e2) = case (simplify e1, simplify e2) of
  (IConst c1, IConst c2) -> IConst (c1 * c2)
  (e1', e2') -> IProd e1' e2'
simplify (IDiv e1 e2) = case (simplify e1, simplify e2) of
  (IConst v1, IConst v2) -> IConst (v1 `div` v2)
  (e1', e2') -> IDiv e1' e2'
simplify (IMod e1 e2) = case (simplify e1, simplify e2) of
  (IConst v1, IConst v2) -> IConst (v1 `mod` v2)
  (e1', e2') -> IMod e1' e2'
simplify (IPow e1 e2) = case (simplify e1, simplify e2) of
  (IConst v1, IConst v2) -> IConst (v1 ^ v2)
  (e1', e2') -> IPow e1' e2'
simplify (IVar name) = IVar name
