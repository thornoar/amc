module Display.Instances.DisplayIEX (display) where
import Object.Bundle

display :: Object IEX -> String
display (IConst v) = show v
display obj = show obj
