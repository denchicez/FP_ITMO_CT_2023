{-# LANGUAGE MultiWayIf #-}
module HW5.Pretty ( prettyValue ) where
import HW5.Base
import Data.Ratio
import Data.Scientific
import Numeric
import Prettyprinter (Doc, pretty, encloseSep)
import Prettyprinter.Render.Terminal
import Data.Foldable (toList)
import Data.ByteString hiding (foldl)
import Data.Word
import Data.Map (toList)


prettyList :: [HiValue] -> Doc AnsiStyle
prettyList arr = encloseSep (pretty "[") (pretty "]") (pretty ", ") (fmap prettyValue arr)

prettyDict :: [(HiValue, HiValue)] -> Doc AnsiStyle
prettyDict arr = encloseSep (pretty "{") (pretty "}") (pretty ", ") (fmap prettyDictArg arr)

prettyWords8 :: [Word8] -> Doc AnsiStyle
prettyWords8 arr = encloseSep (pretty "[# ") (pretty " #]") (pretty " ") (fmap prettyWord8 arr)

prettyDictArg :: (HiValue, HiValue) -> Doc AnsiStyle
prettyDictArg (key, value) = prettyValue key <> pretty ": " <> prettyValue value

prettyWord8 :: Word8 -> Doc AnsiStyle
prettyWord8 word = if word < 16 then pretty "0" <> pretty (showHex word "") else pretty (showHex word "")

-- | Pretty print HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueTime time) = pretty "parse-time(" <> pretty '"' <> pretty (show time) <> pretty '"' <> pretty ")"
prettyValue (HiValueBytes bytes) = prettyWords8 (unpack bytes)
prettyValue (HiValueString str) = pretty '"' <> pretty str <> pretty '"'
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList list) = prettyList (Data.Foldable.toList list)
prettyValue (HiValueNumber number) =  if denominator number == 1
                                      then pretty (numerator number)
                                      else ( case fromRationalRepetendUnlimited number of
                                          (val, Nothing) -> pretty (formatScientific Fixed Nothing val)
                                          _ -> pretty (prettyIrrational number) )
prettyValue (HiValueFunction f) = prettyFun f
prettyValue (HiValueAction action) = prettyAction action
prettyValue (HiValueDict dict) = prettyDict (Data.Map.toList dict)
prettyValue (HiValueBool predicate) = pretty (if predicate then "true" else "false")

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun f = pretty (case f of
  HiFunDiv -> "div"
  HiFunMul -> "mul"
  HiFunSub -> "sub"
  HiFunAdd -> "add"
  HiFunNot -> "not"
  HiFunAnd -> "and"
  HiFunOr -> "or"
  HiFunLessThan -> "less-than"
  HiFunGreaterThan -> "greater-than"
  HiFunEquals -> "equals"
  HiFunNotLessThan -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals -> "not-equals"
  HiFunIf -> "if"
  HiFunLength -> "length"
  HiFunToUpper -> "to-upper"
  HiFunToLower -> "to-lower"
  HiFunReverse -> "reverse"
  HiFunTrim -> "trim"
  HiFunList -> "list"
  HiFunRange -> "range"
  HiFunFold -> "fold"
  HiFunPackBytes -> "pack-bytes"
  HiFunUnpackBytes -> "unpack-bytes"
  HiFunEncodeUtf8 -> "encode-utf8"
  HiFunDecodeUtf8 -> "decode-utf8"
  HiFunZip -> "zip"
  HiFunUnzip -> "unzip"
  HiFunSerialise -> "serialise"
  HiFunDeserialise -> "deserialise"
  HiFunKeys -> "keys"
  HiFunValues -> "values"
  HiFunInvert -> "invert"
  HiFunCount -> "count"
  HiFunRead -> "read"
  HiFunWrite -> "write"
  HiFunMkDir -> "mkdir"
  HiFunChDir -> "cd"
  HiFunParseTime -> "parse-time"
  HiFunRand -> "rand"
  HiFunEcho -> "echo"
  )

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead path) = pretty "read(" <> pretty '"' <> pretty path <> pretty '"' <> pretty ")"
prettyAction (HiActionWrite path bytes) = pretty "write(" <> pretty '"' <> pretty path <> pretty '"' <> pretty ", " <>
                                          prettyValue (HiValueBytes bytes) <> pretty ")"
prettyAction (HiActionMkDir path) = pretty "mkdir(" <> pretty '"' <> pretty path <> pretty '"' <> pretty ")"
prettyAction (HiActionChDir path) = pretty "cd(" <> pretty '"' <> pretty path <> pretty '"' <> pretty ")"
prettyAction HiActionCwd = pretty "cwd"
prettyAction HiActionNow = pretty "now"
prettyAction (HiActionRand left right) = pretty "rand(" <> pretty left <> pretty "," <> pretty right <> pretty ")"
prettyAction (HiActionEcho txt) = pretty "echo" <> pretty "(" <> pretty '"' <> pretty txt <> pretty '"' <> pretty ")"


prettyIrrational :: Rational ->  String
prettyIrrational val = let integralPart = div (numerator val) (denominator val)
                           numVal = numerator val
                           denomVal = denominator val
  in if  | integralPart == 0 || integralPart == -1 -> show numVal ++ "/" ++ show denomVal
         | integralPart < 0 -> show (integralPart+1) ++ " - " ++ prettyFraction denomVal (quotRem (-numVal) denomVal)
         | otherwise -> show integralPart ++ " + " ++  prettyFraction denomVal (quotRem numVal denomVal)

prettyFraction :: Integer -> (Integer, Integer) -> String
prettyFraction num (_, denom) = show denom ++ "/" ++ show num