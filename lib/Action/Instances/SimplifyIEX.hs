module Action.Instances.SimplifyIEX (simplify) where
import Object.Bundle

simplify :: Object IEX -> Object IEX
simplify = preEval

collectTerms :: Object IEX -> [(String, Integer)]
collectTerms = undefined

convertTerm :: [(String, Integer)] -> Object IEX
convertTerm [] = IConst 1
convertTerm ((name,pow):rest) = IProd (IPow (IVar name) (IConst pow)) $ convertTerm rest

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
