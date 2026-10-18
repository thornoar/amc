{-# LANGUAGE UndecidableInstances #-}
module Action.Convert.Bundle (ConvertResult, convertResult) where

import Object.Bundle
import Data.Kind (Constraint)
import Input (IIO, getColoredInputLine, promptLength, splitPair)
import Result
import Description
import Object.RealNumber (RealNumber(Dbl))

data Unit =
    NanoMeter | MicroMeter | MilliMeter | CentiMeter | DeciMeter | Meter | KiloMeter | Mile | Inch | Foot
  | MicroLiter | MilliLiter | CentiLiter | DeciLiter | Liter | DecaLiter | HectoLiter | KiloLiter | Gallon | FluidOunce
  | MicroGram | MilliGram | CentiGram | DeciGram | Gram | DecaGram | HectoGram | KiloGram | Tonne | Ounce | Pound | Stone
  | MicroSecond | MilliSecond | Second | Minute | Hour | Day | Week | Month | Year
  | Bit | Byte | KiloByte | MegaByte | GigaByte | TeraByte | KibiByte | MebiByte | GibiByte | TebiByte
  | Celsius | Fahrenheit | Kelvin | Rankine
  deriving (Eq, Read, Show)

data Dimension = Length | Volume | Mass | Time | Data | Temperature deriving Eq

lengthUnits :: [Unit]
lengthUnits = [NanoMeter, MicroMeter, MilliMeter, CentiMeter, DeciMeter, Meter, KiloMeter, Mile, Inch, Foot]

volumeUnits :: [Unit]
volumeUnits = [MicroLiter, MilliLiter, CentiLiter, DeciLiter, Liter, DecaLiter, HectoLiter, KiloLiter, Gallon, FluidOunce]

massUnits :: [Unit]
massUnits = [MicroGram, MilliGram, CentiGram, DeciGram, Gram, DecaGram, HectoGram, KiloGram, Tonne, Ounce, Pound, Stone]

dataUnits :: [Unit]
dataUnits = [Bit, Byte, KiloByte, MegaByte, GigaByte, TeraByte, KibiByte, MebiByte, GibiByte, TebiByte]

temperatureUnits :: [Unit]
temperatureUnits = [Celsius, Fahrenheit, Kelvin, Rankine]

dimension :: Unit -> Dimension
dimension u
  | u `elem` lengthUnits = Length
  | u `elem` volumeUnits = Volume
  | u `elem` massUnits = Mass
  | u `elem` dataUnits = Data
  | u `elem` temperatureUnits = Temperature
  | otherwise = Time

displayUnit :: Unit -> String
displayUnit NanoMeter = "nanometers"
displayUnit MicroMeter = "micrometers"
displayUnit MilliMeter = "millimeters"
displayUnit CentiMeter = "centimeters"
displayUnit DeciMeter = "decimeters"
displayUnit Meter = "meters"
displayUnit KiloMeter = "kilometers"
displayUnit Mile = "miles"
displayUnit Inch = "inches"
displayUnit Foot = "feet"
displayUnit MicroLiter = "microliters"
displayUnit MilliLiter = "milliliters"
displayUnit CentiLiter = "centiliters"
displayUnit DeciLiter = "deciliters"
displayUnit Liter = "liters"
displayUnit DecaLiter = "decaliters"
displayUnit HectoLiter = "hectoliters"
displayUnit KiloLiter = "kiloliters"
displayUnit Gallon = "gallons"
displayUnit FluidOunce = "fluid ounces"
displayUnit MicroGram = "micrograms"
displayUnit MilliGram = "milligrams"
displayUnit CentiGram = "centigrams"
displayUnit DeciGram = "decigrams"
displayUnit Gram = "grams"
displayUnit DecaGram = "decagrams"
displayUnit HectoGram = "hectograms"
displayUnit KiloGram = "kilograms"
displayUnit Tonne = "tonnes"
displayUnit Ounce = "ounces"
displayUnit Pound = "pounds"
displayUnit Stone = "stones"
displayUnit MicroSecond = "microseconds"
displayUnit MilliSecond = "milliseconds"
displayUnit Second = "seconds"
displayUnit Minute = "minutes"
displayUnit Hour = "hours"
displayUnit Day = "days"
displayUnit Week = "weeks"
displayUnit Month = "months"
displayUnit Year = "years"
displayUnit Bit = "bits"
displayUnit Byte = "bytes"
displayUnit KiloByte = "kilobytes"
displayUnit MegaByte = "megabytes"
displayUnit GigaByte = "gigabytes"
displayUnit TeraByte = "terabytes"
displayUnit KibiByte = "kibibytes"
displayUnit MebiByte = "mebibytes"
displayUnit GibiByte = "gibibytes"
displayUnit TebiByte = "tebibytes"
displayUnit Celsius = "degrees Celsius"
displayUnit Fahrenheit = "degrees Fahrenheit"
displayUnit Kelvin = "kelvin"
displayUnit Rankine = "degrees Rankine"

toBase :: Unit -> (Double, Double)
toBase NanoMeter = (1.0e-7, 0.0)
toBase MicroMeter = (1.0e-4, 0.0)
toBase MilliMeter = (0.1, 0.0)
toBase CentiMeter = (1.0, 0.0)
toBase DeciMeter = (10.0, 0.0)
toBase Meter = (100.0, 0.0)
toBase KiloMeter = (100000.0, 0.0)
toBase Mile = (160934.4, 0.0)
toBase Inch = (2.54, 0.0)
toBase Foot = (30.48, 0.0)
toBase MicroLiter = (1.0e-6, 0.0)
toBase MilliLiter = (1.0e-3, 0.0)
toBase CentiLiter = (1.0e-2, 0.0)
toBase DeciLiter = (0.1, 0.0)
toBase Liter = (1.0, 0.0)
toBase DecaLiter = (10.0, 0.0)
toBase HectoLiter = (100.0, 0.0)
toBase KiloLiter = (1000.0, 0.0)
toBase Gallon = (3.785411784, 0.0)
toBase FluidOunce = (0.0295735295625, 0.0)
toBase MicroGram = (1.0e-6, 0.0)
toBase MilliGram = (1.0e-3, 0.0)
toBase CentiGram = (1.0e-2, 0.0)
toBase DeciGram = (0.1, 0.0)
toBase Gram = (1.0, 0.0)
toBase DecaGram = (10.0, 0.0)
toBase HectoGram = (100.0, 0.0)
toBase KiloGram = (1000.0, 0.0)
toBase Tonne = (1.0e6, 0.0)
toBase Ounce = (28.349523125, 0.0)
toBase Pound = (453.59237, 0.0)
toBase Stone = (6350.29318, 0.0)
toBase MicroSecond = (1.0e-6, 0.0)
toBase MilliSecond = (1.0e-3, 0.0)
toBase Second = (1.0, 0.0)
toBase Minute = (60.0, 0.0)
toBase Hour = (3600.0, 0.0)
toBase Day = (86400.0, 0.0)
toBase Week = (604800.0, 0.0)
toBase Month = (2629800.0, 0.0)
toBase Year = (31557600.0, 0.0)
toBase Bit = (0.125, 0.0)
toBase Byte = (1.0, 0.0)
toBase KiloByte = (1000.0, 0.0)
toBase MegaByte = (1.0e6, 0.0)
toBase GigaByte = (1.0e9, 0.0)
toBase TeraByte = (1.0e12, 0.0)
toBase KibiByte = (1024.0, 0.0)
toBase MebiByte = (1048576.0, 0.0)
toBase GibiByte = (1073741824.0, 0.0)
toBase TebiByte = (1099511627776.0, 0.0)
toBase Celsius = (1.0, 273.15)
toBase Fahrenheit = (5/9, 273.15 - 160/9)
toBase Kelvin = (1.0, 0.0)
toBase Rankine = (5/9, 0.0)

convertFactor :: (Unit, Unit) -> Result (Double, Double)
convertFactor (u, u')
  | u == u' = Content (1.0, 0.0)
  | dimension u /= dimension u' = Error $ displayUnit u ++ " cannot be converted to " ++ displayUnit u'
  | otherwise = Content (scale, offset)
  where
    (su, ou) = toBase u
    (su', ou') = toBase u'
    scale = su / su'
    offset = (ou - ou') / su'

unitAliases :: [(String, Unit)]
unitAliases =
  [ ("nm", NanoMeter), ("nanometer", NanoMeter), ("nanometers", NanoMeter), ("nanometre", NanoMeter), ("nanometres", NanoMeter)
  , ("um", MicroMeter), ("µm", MicroMeter), ("micrometer", MicroMeter), ("micrometers", MicroMeter), ("micrometre", MicroMeter), ("micrometres", MicroMeter)
  , ("mm", MilliMeter), ("millimeter", MilliMeter), ("millimeters", MilliMeter), ("millimetre", MilliMeter), ("millimetres", MilliMeter)
  , ("cm", CentiMeter), ("centimeter", CentiMeter), ("centimeters", CentiMeter), ("centimetre", CentiMeter), ("centimetres", CentiMeter)
  , ("dm", DeciMeter), ("decimeter", DeciMeter), ("decimeters", DeciMeter), ("decimetre", DeciMeter), ("decimetres", DeciMeter)
  , ("m", Meter), ("meter", Meter), ("meters", Meter), ("metre", Meter), ("metres", Meter)
  , ("km", KiloMeter), ("kilometer", KiloMeter), ("kilometers", KiloMeter), ("kilometre", KiloMeter), ("kilometres", KiloMeter)
  , ("mi", Mile), ("mile", Mile), ("miles", Mile)
  , ("in", Inch), ("inch", Inch), ("inches", Inch)
  , ("ft", Foot), ("foot", Foot), ("feet", Foot)
  , ("ul", MicroLiter), ("µl", MicroLiter), ("microliter", MicroLiter), ("microliters", MicroLiter), ("microlitre", MicroLiter), ("microlitres", MicroLiter)
  , ("ml", MilliLiter), ("milliliter", MilliLiter), ("milliliters", MilliLiter), ("millilitre", MilliLiter), ("millilitres", MilliLiter)
  , ("cl", CentiLiter), ("centiliter", CentiLiter), ("centiliters", CentiLiter), ("centilitre", CentiLiter), ("centilitres", CentiLiter)
  , ("dl", DeciLiter), ("deciliter", DeciLiter), ("deciliters", DeciLiter), ("decilitre", DeciLiter), ("decilitres", DeciLiter)
  , ("l", Liter), ("liter", Liter), ("liters", Liter), ("litre", Liter), ("litres", Liter)
  , ("dal", DecaLiter), ("decaliter", DecaLiter), ("decaliters", DecaLiter), ("decalitre", DecaLiter), ("decalitres", DecaLiter)
  , ("hl", HectoLiter), ("hectoliter", HectoLiter), ("hectoliters", HectoLiter), ("hectolitre", HectoLiter), ("hectolitres", HectoLiter)
  , ("kl", KiloLiter), ("kiloliter", KiloLiter), ("kiloliters", KiloLiter), ("kilolitre", KiloLiter), ("kilolitres", KiloLiter)
  , ("gal", Gallon), ("gallon", Gallon), ("gallons", Gallon)
  , ("floz", FluidOunce), ("fluidounce", FluidOunce), ("fluidounces", FluidOunce)
  , ("ug", MicroGram), ("µg", MicroGram), ("microgram", MicroGram), ("micrograms", MicroGram), ("microgramme", MicroGram), ("microgrammes", MicroGram)
  , ("mg", MilliGram), ("milligram", MilliGram), ("milligrams", MilliGram), ("milligramme", MilliGram), ("milligrammes", MilliGram)
  , ("cg", CentiGram), ("centigram", CentiGram), ("centigrams", CentiGram), ("centigramme", CentiGram), ("centigrammes", CentiGram)
  , ("dg", DeciGram), ("decigram", DeciGram), ("decigrams", DeciGram), ("decigramme", DeciGram), ("decigrammes", DeciGram)
  , ("g", Gram), ("gram", Gram), ("grams", Gram), ("gramme", Gram), ("grammes", Gram)
  , ("dag", DecaGram), ("decagram", DecaGram), ("decagrams", DecaGram), ("decagramme", DecaGram), ("decagrammes", DecaGram)
  , ("hg", HectoGram), ("hectogram", HectoGram), ("hectograms", HectoGram), ("hectogramme", HectoGram), ("hectogrammes", HectoGram)
  , ("kg", KiloGram), ("kilogram", KiloGram), ("kilograms", KiloGram), ("kilogramme", KiloGram), ("kilogrammes", KiloGram)
  , ("t", Tonne), ("tonne", Tonne), ("tonnes", Tonne), ("ton", Tonne), ("tons", Tonne)
  , ("oz", Ounce), ("ounce", Ounce), ("ounces", Ounce)
  , ("lb", Pound), ("lbs", Pound), ("pound", Pound), ("pounds", Pound)
  , ("st", Stone), ("stone", Stone), ("stones", Stone)
  , ("us", MicroSecond), ("µs", MicroSecond), ("microsecond", MicroSecond), ("microseconds", MicroSecond)
  , ("ms", MilliSecond), ("millisecond", MilliSecond), ("milliseconds", MilliSecond)
  , ("s", Second), ("sec", Second), ("secs", Second), ("second", Second), ("seconds", Second)
  , ("min", Minute), ("mins", Minute), ("minute", Minute), ("minutes", Minute)
  , ("h", Hour), ("hr", Hour), ("hrs", Hour), ("hour", Hour), ("hours", Hour)
  , ("d", Day), ("day", Day), ("days", Day)
  , ("w", Week), ("wk", Week), ("week", Week), ("weeks", Week)
  , ("mo", Month), ("mon", Month), ("month", Month), ("months", Month)
  , ("y", Year), ("yr", Year), ("yrs", Year), ("year", Year), ("years", Year)
  , ("b", Bit), ("bit", Bit), ("bits", Bit)
  , ("B", Byte), ("byte", Byte), ("bytes", Byte)
  , ("KB", KiloByte), ("kB", KiloByte), ("kilobyte", KiloByte), ("kilobytes", KiloByte)
  , ("MB", MegaByte), ("megabyte", MegaByte), ("megabytes", MegaByte)
  , ("GB", GigaByte), ("gigabyte", GigaByte), ("gigabytes", GigaByte)
  , ("TB", TeraByte), ("terabyte", TeraByte), ("terabytes", TeraByte)
  , ("KiB", KibiByte), ("kibibyte", KibiByte), ("kibibytes", KibiByte)
  , ("MiB", MebiByte), ("mebibyte", MebiByte), ("mebibytes", MebiByte)
  , ("GiB", GibiByte), ("gibibyte", GibiByte), ("gibibytes", GibiByte)
  , ("TiB", TebiByte), ("tebibyte", TebiByte), ("tebibytes", TebiByte)
  , ("c", Celsius), ("celsius", Celsius), ("°c", Celsius), ("degc", Celsius), ("centigrade", Celsius)
  , ("f", Fahrenheit), ("fahrenheit", Fahrenheit), ("°f", Fahrenheit), ("degf", Fahrenheit)
  , ("k", Kelvin), ("kelvin", Kelvin)
  , ("r", Rankine), ("rankine", Rankine), ("°r", Rankine), ("degr", Rankine)
  ]

guessUnit :: String -> Result Unit
guessUnit str = case lookup str unitAliases of
  Just u -> Content u
  Nothing -> Error $ "could not recognize unit: `" ++ str ++ "`"

getUnits :: IIO (Result (Unit, Unit))
getUnits = do
  let prstr = " from > to: "
      go (ustr1, ustr2) = (,) <$> guessUnit ustr1 <*> guessUnit ustr2
  input <- getColoredInputLine $ replicate (promptLength - length prstr) '#' ++ prstr
  return $ input >>= go . splitPair False (== '>') . filter (/= ' ')

type ConvertResult :: ObjectTag -> Constraint
class ConvertResult tg where
  convertResult :: Object tg -> IIO (Result (Object tg))

instance {-# OVERLAPPABLE #-} Description tg => ConvertResult tg where
  convertResult obj = return . Error $ (description $ proxyOf obj) ++ " cannot be used for unit conversion"

instance {-# OVERLAPPING #-} ConvertResult IEX where
  convertResult obj = getUnits >>= return . (fmap apply) . (>>= convertFactor)
    where
      apply (scale, offset)
        | offset == 0.0 = IProd obj (IConst (floor scale))
        | otherwise = ISum (IProd obj (IConst (floor scale))) (IConst (floor offset))

instance {-# OVERLAPPING #-} ConvertResult REX where
  convertResult obj = getUnits >>= return . (fmap apply) . (>>= convertFactor)
    where
      apply (scale, offset)
        | offset == 0.0 = RProd obj (RConst (Dbl scale))
        | otherwise = RSum (RProd obj (RConst (Dbl scale))) (RConst (Dbl offset))
