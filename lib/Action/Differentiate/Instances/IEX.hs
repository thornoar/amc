module Action.Differentiate.Instances.IEX (diff) where

import Object.Bundle
import Result

diff :: String -> Object IEX -> Result (Object IEX)
diff _ (IConst _) = Content (IConst 0)
diff var (INeg expr) = INeg <$> diff var expr
diff var (ISum e1 e2) = ISum <$> diff var e1 <*> diff var e2
diff var (IProd e1 e2) =
  ISum <$> (IProd <$> diff var e1 <*> pure e2)
       <*> (IProd e1 <$> diff var e2)
diff var (IDiv e1 e2) =
  IDiv <$> (ISum <$> (IProd <$> diff var e1 <*> pure e2)
                 <*> (INeg <$> (IProd e1 <$> diff var e2)))
       <*> pure (IPow e2 (IConst 2))
diff var (IMod e1 e2)
  | dependsOn var e1 || dependsOn var e2 =
      Error "a modulus expression cannot be differentiated"
  | otherwise = Content (IConst 0)
diff var (IPow e1 e2)
  | dependsOn var e2 =
      Error "an expression with a variable exponent cannot be differentiated"
  | otherwise =
      IProd <$> (IProd e2 <$> pure (IPow e1 (ISum e2 (IConst (-1)))))
            <*> diff var e1
diff var (IVar name)
  | name == var = Content (IConst 1)
  | otherwise = Content (IConst 0)

-- | Whether an integer expression depends on the given variable.
dependsOn :: String -> Object IEX -> Bool
dependsOn _ (IConst _) = False
dependsOn var (INeg expr) = dependsOn var expr
dependsOn var (ISum e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (IProd e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (IDiv e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (IMod e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (IPow e1 e2) = dependsOn var e1 || dependsOn var e2
dependsOn var (IVar name) = name == var
