{-# LANGUAGE TupleSections #-}
module HW5.Evaluator ( eval ) where

import HW5.Base (HiFun (..), HiError (..), HiExpr (..), HiValue (..), HiAction(..), HiMonad(..))
import Data.Text as T
import Data.Ratio
import Control.Monad.Trans.Except (runExceptT, ExceptT(..), throwE)
import Control.Monad (when, unless, foldM)
import Data.Semigroup (stimes)
import Data.Sequence as S
import Data.Foldable (toList)
import Data.ByteString as BS (pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word
import Data.Map as M (fromList, findWithDefault, keys, elems, toList, fromListWith, Map, adjust)
import Data.Text.Encoding
import Codec.Compression.Zlib
import Codec.Serialise
import Data.Time.Clock
import Text.Read (readMaybe)

exprToApplyValue :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
exprToApplyValue expr = do
  value <- evalExceptT expr
  case value of
    HiValueFunction fun -> pure (HiValueFunction fun)
    HiValueString str -> pure (HiValueString str)
    HiValueList list -> pure (HiValueList list)
    HiValueBytes bytes -> pure (HiValueBytes bytes)
    HiValueDict dict -> pure (HiValueDict dict)
    _ -> throwE HiErrorInvalidFunction

rationalToInt :: HiMonad m => Rational -> ExceptT HiError m Int
rationalToInt val = case (numerator val, denominator val) of
  (integralPart, 1) -> pure (fromInteger integralPart)
  _ -> throwE HiErrorInvalidArgument

evaluateN :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evaluateN (HiValueFunction HiFunNot) [HiValueBool val] = pure (HiValueBool (not val))
evaluateN (HiValueFunction HiFunLength) [HiValueString val] = pure (HiValueNumber (toRational (T.length val)))
evaluateN (HiValueFunction HiFunToUpper) [HiValueString val] = pure (HiValueString (T.toUpper val))
evaluateN (HiValueFunction HiFunToLower) [HiValueString val] = pure (HiValueString (T.toLower val))
evaluateN (HiValueFunction HiFunReverse) [HiValueString val] = pure (HiValueString (T.reverse val))
evaluateN (HiValueFunction HiFunTrim) [HiValueString val] = pure (HiValueString (T.strip val))
evaluateN (HiValueString str) [HiValueNumber val] = do
  convertValToInt <- rationalToInt val
  let endpoint = convertValToInt + 1
  if endpoint <= 0 || endpoint > T.length str
  then pure HiValueNull
  else pure (HiValueString (T.drop convertValToInt (T.take endpoint str)))
evaluateN (HiValueFunction HiFunDiv) [HiValueNumber _, HiValueNumber 0] = throwE HiErrorDivideByZero
evaluateN (HiValueFunction HiFunDiv) [HiValueNumber left, HiValueNumber right] = pure (HiValueNumber (left / right))
evaluateN (HiValueFunction HiFunDiv) [HiValueString left, HiValueString right] =
  pure (HiValueString (left <> T.pack "/" <> right))
evaluateN (HiValueFunction HiFunMul) [HiValueNumber left, HiValueNumber right] =
  pure (HiValueNumber (left * right))
evaluateN (HiValueFunction HiFunMul) [HiValueString str, HiValueNumber n] = do
  nInt <- rationalToInt n
  when (nInt <= 0) (throwE HiErrorInvalidArgument)
  pure (HiValueString (stimes nInt str))
evaluateN (HiValueFunction HiFunSub) [HiValueNumber left, HiValueNumber right] = pure (HiValueNumber (left - right))
evaluateN (HiValueFunction HiFunAdd) [HiValueNumber left, HiValueNumber right] = pure (HiValueNumber (left + right))
evaluateN (HiValueFunction HiFunAdd) [HiValueString left, HiValueString right] =
  pure (HiValueString (T.concat [left, right]))
evaluateN (HiValueFunction HiFunAnd) [HiValueBool left, HiValueBool right] = pure (HiValueBool (left && right))
evaluateN (HiValueFunction HiFunOr) [HiValueBool left, HiValueBool right] = pure (HiValueBool (left || right))
evaluateN (HiValueFunction HiFunEquals) [left, right] = pure (HiValueBool (left == right))
evaluateN (HiValueFunction HiFunLessThan) [left, right] = pure (HiValueBool (left < right))
evaluateN (HiValueFunction HiFunGreaterThan) [left, right] = evaluateN (HiValueFunction HiFunLessThan) [right, left]
evaluateN (HiValueFunction HiFunNotEquals) [left, right] = evaluateNot (HiValueFunction HiFunEquals) [left, right]
evaluateN (HiValueFunction HiFunNotLessThan) [left, right] = evaluateNot (HiValueFunction HiFunLessThan) [left, right]
evaluateN (HiValueFunction HiFunNotGreaterThan) [left, right] =
  evaluateNot (HiValueFunction HiFunGreaterThan) [left, right]
evaluateN (HiValueString str) [HiValueNumber left, HiValueNumber right] = do
  leftBorder <- rationalToInt left
  let leftBorderUpd = if leftBorder < 0 then leftBorder + T.length str else leftBorder
  rightBorder <- rationalToInt right
  let rightBorderUpd = if rightBorder < 0 then rightBorder + T.length str else rightBorder
  pure (HiValueString (T.drop leftBorderUpd (T.take rightBorderUpd str)))
evaluateN (HiValueString str) [HiValueNull, HiValueNumber right] =
  evaluateN (HiValueString str) [HiValueNumber (toRational (0 :: Integer)), HiValueNumber right]
evaluateN (HiValueString str) [HiValueNumber left, HiValueNull] =
  evaluateN (HiValueString str) [HiValueNumber left, HiValueNumber (toRational (T.length str))]
evaluateN (HiValueString str) [HiValueNull, HiValueNull] =
  evaluateN (HiValueString str) [
    HiValueNumber (toRational (0 :: Integer)),
    HiValueNumber (toRational (T.length str)) ]
evaluateN (HiValueFunction HiFunIf) [HiValueBool True, left, _] = pure left
evaluateN (HiValueFunction HiFunIf) [HiValueBool False, _, right] = pure right
evaluateN (HiValueFunction HiFunList) arr = pure (HiValueList (S.fromList arr))
evaluateN (HiValueFunction HiFunRange) [HiValueNumber left, HiValueNumber right] =
  pure (HiValueList (fmap HiValueNumber (S.fromList [left..right])))
evaluateN (HiValueFunction HiFunFold) [act, HiValueList arr] = foldMapArgs act (Data.Foldable.toList arr)
evaluateN (HiValueFunction HiFunLength) [HiValueList arr] = pure (HiValueNumber (toRational (S.length arr)))
evaluateN (HiValueFunction HiFunReverse) [HiValueList arr] = pure (HiValueList (S.reverse arr))
evaluateN (HiValueFunction HiFunAdd) [HiValueList left, HiValueList right] = pure (HiValueList (left >< right))
evaluateN (HiValueFunction HiFunMul) [HiValueList arr, HiValueNumber n] = do
  nInt <- rationalToInt n
  when (nInt <= 0) (throwE HiErrorInvalidArgument)
  pure (HiValueList (stimes nInt arr))
evaluateN (HiValueList arr) [HiValueNumber leftBorder, HiValueNumber rightBorder] = do
  convertLeftBorder <- rationalToInt leftBorder
  convertRightBorder <- rationalToInt rightBorder
  pure (HiValueList (S.drop convertLeftBorder (S.take convertRightBorder arr)))

evaluateN (HiValueList arr) [HiValueNumber endpoint] = do
  endpointInt <- rationalToInt endpoint
  if 0 > endpointInt || endpointInt >= S.length arr then pure HiValueNull else pure (arr `S.index` endpointInt)
evaluateN (HiValueFunction HiFunPackBytes) [HiValueList arr] = do
  words8 <- mapM convertIntToByte arr
  pure (HiValueBytes (BS.pack (Data.Foldable.toList words8)))
evaluateN (HiValueFunction HiFunUnpackBytes) [HiValueBytes bs] = do
  let words8 = BS.unpack bs
  let words8ToInt = Prelude.map convertByteToInt words8
  pure (HiValueList (S.fromList words8ToInt))
evaluateN (HiValueFunction HiFunEncodeUtf8) [HiValueString str] = pure (HiValueBytes (encodeUtf8 str))
evaluateN (HiValueFunction HiFunDecodeUtf8) [HiValueBytes bs] = case decodeUtf8' bs of
  Left _ -> pure HiValueNull
  Right str -> pure (HiValueString str)

evaluateN (HiValueFunction HiFunZip) [HiValueBytes bs] = do
  let compressSettings = defaultCompressParams {compressLevel = bestCompression}
  pure (HiValueBytes (toStrict (compressWith compressSettings (fromStrict bs))))
evaluateN (HiValueFunction HiFunUnzip) [HiValueBytes bs] =
  pure (HiValueBytes (toStrict (decompressWith defaultDecompressParams (fromStrict bs))))

evaluateN (HiValueFunction HiFunSerialise) [arg] = pure (HiValueBytes (toStrict (serialise arg)))
evaluateN (HiValueFunction HiFunDeserialise) [HiValueBytes bs] = pure (deserialise (fromStrict bs))
evaluateN (HiValueFunction HiFunAdd) [HiValueBytes left, HiValueBytes right] = pure (HiValueBytes (mappend left right))
evaluateN (HiValueFunction HiFunMul) [HiValueBytes arr, HiValueNumber n] = do
  nInt <- rationalToInt n
  pure (HiValueBytes (stimes nInt arr))
evaluateN (HiValueBytes bs) [HiValueNumber endpoint] = do
  endpointInt <- rationalToInt endpoint
  let words8 = BS.unpack bs
  let words8ToInt = Prelude.map convertByteToInt words8
  let words8List = S.fromList words8ToInt
  if 0 > endpointInt || endpointInt >= S.length words8List
    then pure HiValueNull
    else pure (words8List `S.index` endpointInt)

evaluateN (HiValueBytes bs) [HiValueNumber leftBorder, HiValueNumber rightBorder] = do
  convertLeftBorder <- rationalToInt leftBorder
  convertRightBorder <- rationalToInt rightBorder
  let words8 = BS.unpack bs
  let firstBytes = S.take convertRightBorder (S.fromList words8)
  pure (HiValueBytes (BS.pack (Data.Foldable.toList (S.drop convertLeftBorder firstBytes))))
evaluateN (HiValueDict dict) [key] = pure (findWithDefault HiValueNull key dict)
evaluateN (HiValueFunction HiFunKeys) [HiValueDict dict] = pure (HiValueList (S.fromList (keys dict)))
evaluateN (HiValueFunction HiFunValues) [HiValueDict dict] = pure (HiValueList (S.fromList (elems dict)))
evaluateN (HiValueFunction HiFunInvert) [HiValueDict dict] = do
  let arrMap = invertPairArr (M.toList dict)
  let newMap = fromListWith (++) arrMap
  let newDict = pairToMap (M.toList newMap)
  pure (HiValueDict newDict)

evaluateN (HiValueFunction HiFunCount) [HiValueString str] = do
  let mapCharInt = convertArrToDict (T.unpack str)
  let listChar = M.toList mapCharInt
  let listPair = Prelude.map (\(l, r) -> (HiValueString (T.singleton l), HiValueNumber (toRational r))) listChar
  let getMap = M.fromList listPair
  pure (HiValueDict getMap)
evaluateN (HiValueFunction HiFunCount) [HiValueList arr] = do
  let arr2 = Data.Foldable.toList arr
  let mapValueInt = convertArrToDict arr2
  let getMap = M.fromList (Prelude.map (\(l, r) -> (l, HiValueNumber (toRational r))) (M.toList mapValueInt))
  pure (HiValueDict getMap)
evaluateN (HiValueFunction HiFunCount) [HiValueBytes bs] = do
  let words8 = BS.unpack bs
  let convertedWords = Prelude.map convertByteToInt words8
  let mapValueInt = convertArrToDict convertedWords
  let getMap = M.fromList (Prelude.map (\(l, r) -> (l, HiValueNumber (toRational r))) (M.toList mapValueInt))
  pure (HiValueDict getMap)
evaluateN (HiValueFunction HiFunRead) [HiValueString s] = pure (HiValueAction (HiActionRead (T.unpack s)))
evaluateN (HiValueFunction HiFunWrite) [HiValueString s, HiValueString txt] = do
  pure (HiValueAction (HiActionWrite (T.unpack s) (encodeUtf8 txt)))
evaluateN (HiValueFunction HiFunMkDir) [HiValueString s] = pure (HiValueAction (HiActionMkDir (T.unpack s)))
evaluateN (HiValueFunction HiFunChDir) [HiValueString s] = pure (HiValueAction (HiActionChDir (T.unpack s)))
evaluateN (HiValueFunction HiFunParseTime) [HiValueString s] = do
  case readMaybe (T.unpack s) of
    Just x -> pure (HiValueTime x)
    _ -> pure HiValueNull
evaluateN (HiValueFunction HiFunAdd) [HiValueTime left, HiValueNumber val] =
  pure (HiValueTime (addUTCTime (fromRational val) left))
evaluateN (HiValueFunction HiFunSub) [HiValueTime left, HiValueTime right] =
  pure (HiValueNumber (toRational (diffUTCTime left right)))
evaluateN (HiValueFunction HiFunRand) [HiValueNumber left, HiValueNumber right] = do
  leftInt <- rationalToInt left
  rightInt <- rationalToInt right
  pure (HiValueAction (HiActionRand leftInt rightInt))
evaluateN (HiValueFunction HiFunEcho) [HiValueString val] = pure (HiValueAction (HiActionEcho val))
evaluateN _ _ = throwE HiErrorInvalidArgument

convertArrToDict :: Ord a => [a] -> Map a Int
convertArrToDict arr = do
  let res = M.fromList (Prelude.map (, 0) arr)
  Prelude.foldl (flip (M.adjust (1 +))) res arr

invertPairArr :: [(HiValue, HiValue)] -> [(HiValue, [HiValue])]
invertPairArr = Prelude.map (\(left, right) -> (right, [left]))

pairToMap :: [(HiValue, [HiValue])] -> Map HiValue HiValue
pairToMap arr = M.fromList (Prelude.map (\(left, right) -> (left, HiValueList (S.fromList right))) arr)

convertIntToByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
convertIntToByte (HiValueNumber val) = do
  getHex <- rationalToInt val
  pure (toEnum getHex)
convertIntToByte _ = throwE HiErrorInvalidArgument

convertByteToInt :: Word8 -> HiValue
convertByteToInt val = HiValueNumber (toRational (fromIntegral val :: Integer))

foldMapArgs :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
foldMapArgs applyValue (x: xs) = do
  unless (2 `elem` getArity applyValue) (throwE HiErrorInvalidArgument)
  foldM (\ left right -> evaluateN applyValue [left, right]) x xs
foldMapArgs _ _ = pure HiValueNull

evaluateNot :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evaluateNot applyValue argsValue = do
  evalValue <- evaluateN applyValue argsValue
  evaluateN (HiValueFunction HiFunNot) [evalValue]

-- | Convert HiExpr to HiValue with HiError
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalExceptT expr)

evalExceptT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExceptT (HiExprValue value) = pure value
evalExceptT (HiExprRun expr) = do
  value <- evalExceptT expr
  evalOnlyAction value
evalExceptT (HiExprApply apply args) = evalExprApply apply args
evalExceptT (HiExprDict dict) = evalExprDict dict

evalOnlyAction :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalOnlyAction (HiValueAction action) = ExceptT (fmap Right (runAction action))
evalOnlyAction _ = throwE HiErrorInvalidArgument

evalExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalExprApply applyExpr argsExpr = do
  applyValue <- exprToApplyValue applyExpr
  unless (Prelude.length argsExpr `elem` getArity applyValue) (throwE HiErrorArityMismatch)
  evalLazy applyValue argsExpr

evalLazy :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalLazy (HiValueFunction HiFunAnd) [leftExpr, rightExpr] = do
  leftValue <- evalExceptT leftExpr
  if leftValue == HiValueNull || leftValue == HiValueBool False
    then pure leftValue
    else do evalExceptT rightExpr

evalLazy (HiValueFunction HiFunOr) [leftExpr, rightExpr] = do
  leftValue <- evalExceptT leftExpr
  if not (leftValue == HiValueNull || leftValue == HiValueBool False)
    then pure leftValue
    else do evalExceptT rightExpr

evalLazy (HiValueFunction HiFunIf) [predicator, left, right] = do
  evalPredicator <- evalExceptT predicator
  evalLazyIf (HiValueFunction HiFunIf) evalPredicator [left, right]

evalLazy applyValue argsExpr = do
  argsValue <- mapM evalExceptT argsExpr
  evaluateN applyValue argsValue

evalLazyIf :: HiMonad m => HiValue -> HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalLazyIf (HiValueFunction HiFunIf) (HiValueBool True) [left, _] =  evalExceptT left
evalLazyIf (HiValueFunction HiFunIf) (HiValueBool False) [_, right] = evalExceptT right
evalLazyIf _ _ _ = throwE HiErrorInvalidArgument

evalExprDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evalExprDict exprValuesDict = do
  evalExprValuesDict <- mapM evalExprDictValue exprValuesDict
  pure (HiValueDict (M.fromList evalExprValuesDict))

evalExprDictValue :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalExprDictValue (keyExpr, valueExpr) = do
  keyValue <- evalExceptT keyExpr
  valueValue <- evalExceptT valueExpr
  pure (keyValue, valueValue)


getArity :: HiValue -> [Int]
getArity (HiValueFunction HiFunDiv) = [2]
getArity (HiValueFunction HiFunMul) = [2]
getArity (HiValueFunction HiFunAdd) = [2]
getArity (HiValueFunction HiFunSub) = [2]
getArity (HiValueFunction HiFunNot) = [1]
getArity (HiValueFunction HiFunAnd) = [2]
getArity (HiValueFunction HiFunOr) = [2]
getArity (HiValueFunction HiFunLessThan) = [2]
getArity (HiValueFunction HiFunGreaterThan) = [2]
getArity (HiValueFunction HiFunEquals) = [2]
getArity (HiValueFunction HiFunNotLessThan) = [2]
getArity (HiValueFunction HiFunNotGreaterThan) = [2]
getArity (HiValueFunction HiFunNotEquals) = [2]
getArity (HiValueFunction HiFunIf) = [3]
getArity (HiValueFunction HiFunLength) = [1]
getArity (HiValueFunction HiFunToUpper) = [1]
getArity (HiValueFunction HiFunToLower) = [1]
getArity (HiValueFunction HiFunReverse) = [1]
getArity (HiValueFunction HiFunTrim) = [1]
getArity (HiValueString _) = [1, 2]
getArity (HiValueFunction HiFunList) = [0..]
getArity (HiValueFunction HiFunRange) = [2]
getArity (HiValueFunction HiFunFold) = [2]
getArity (HiValueList _) = [1, 2]
getArity (HiValueFunction HiFunPackBytes) = [1]
getArity (HiValueFunction HiFunUnpackBytes) = [1]
getArity (HiValueFunction HiFunEncodeUtf8) = [1]
getArity (HiValueFunction HiFunDecodeUtf8) = [1]
getArity (HiValueFunction HiFunZip) = [1]
getArity (HiValueFunction HiFunUnzip) = [1]
getArity (HiValueFunction HiFunSerialise) = [1]
getArity (HiValueFunction HiFunDeserialise) = [1]
getArity (HiValueBytes _) = [1, 2]
getArity (HiValueDict _) = [0..]
getArity (HiValueFunction HiFunKeys) = [1]
getArity (HiValueFunction HiFunValues) = [1]
getArity (HiValueFunction HiFunInvert) = [1]
getArity (HiValueFunction HiFunCount) = [1]
getArity (HiValueFunction HiFunRead) = [1]
getArity (HiValueFunction HiFunWrite) = [2]
getArity (HiValueFunction HiFunMkDir) = [1]
getArity (HiValueFunction HiFunChDir) = [1]
getArity (HiValueAction HiActionCwd) = []
getArity (HiValueFunction HiFunParseTime) = [1]
getArity (HiValueFunction HiFunRand) = [2]
getArity (HiValueFunction HiFunEcho) = [1]
getArity _ = []