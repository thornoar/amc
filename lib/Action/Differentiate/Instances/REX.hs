module Action.Differentiate.Instances.REX (diff) where

import Object.Bundle
import Object.RealNumber
import Result

diff :: String -> Object REX -> Result (Object REX)
diff _ (RConst _) = Content (RConst (In 0))
diff var (RNeg expr) = RNeg <$> diff var expr
diff var (RSum e1 e2) = RSum <$> diff var e1 <*> diff var e2
diff var (RInv expr) =
  RNeg <$> (RProd <$> (RInv <$> pure (RPow expr (RConst (In 2))))
                 <*> diff var expr)
diff var (RProd e1 e2) =
  RSum <$> (RProd <$> diff var e1 <*> pure e2)
       <*> (RProd e1 <$> diff var e2)
diff var (RDiv e1 e2) =
  RDiv <$> (RSum <$> (RProd <$> diff var e1 <*> pure e2)
                 <*> (RNeg <$> (RProd e1 <$> diff var e2)))
       <*> pure (RPow e2 (RConst (In 2)))
diff var (RLog base arg)
  | dependsOn var base =
      RDiv <$> (RSum <$> (RDiv <$> (RProd <$> diff var arg <*> pure (RApp Ln base))
                                <*> pure arg)
                      <*> (RNeg <$> (RProd <$> pure (RApp Ln arg)
                                           <*> (RDiv <$> diff var base <*> pure base))))
            <*> pure (RPow (RApp Ln base) (RConst (In 2)))
  | otherwise =
      RDiv <$> diff var arg
           <*> pure (RProd arg (RApp Ln base))
diff var (RPow base expo)
  | dependsOn var expo =
      RProd <$> pure (RPow base expo)
            <*> (RSum <$> (RProd <$> diff var expo <*> pure (RApp Ln base))
                      <*> (RDiv <$> (RProd <$> pure expo <*> diff var base)
                                <*> pure base))
  | otherwise =
      RProd <$> (RProd expo <$> pure (RPow base (RSum expo (RConst (In (-1))))))
            <*> diff var base
diff var (RVar name)
  | name == var = Content (RConst (In 1))
  | otherwise = Content (RConst (In 0))
diff var (RApp fun expr) =
  RProd <$> pure (diffApp fun expr) <*> diff var expr

-- | The derivative of a builtin function with respect to its argument.
diffApp :: BuiltinFunction -> Object REX -> Object REX
diffApp Exp expr = RApp Exp expr
diffApp Ln expr = RInv expr
diffApp Sin expr = RApp Cos expr
diffApp Cos expr = RNeg (RApp Sin expr)
diffApp Tan expr = RInv (RPow (RApp Cos expr) (RConst (In 2)))
diffApp Cot expr = RNeg (RInv (RPow (RApp Sin expr) (RConst (In 2))))

-- | Whether a real expression depends on the given variable.
dependsOn :: String -> Object REX -> Bool
dependsOn _ (RConst _) = False
dependsOn var (RNeg expr) = dependsOn var expr
dependsOn var (RSum e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (RInv expr) = dependsOn var expr
dependsOn var (RProd e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (RDiv e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (RLog e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (RPow e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (RVar name) = name == var
dependsOn var (RApp _ expr) = dependsOn var expr
