{-# LANGUAGE OverloadedStrings #-}
module HW5.Parser ( parse ) where
import Data.Void (Void)
import Data.Scientific
import Text.Megaparsec as MP hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text as T
import Text.Read (readMaybe)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void)
import HW5.Base (HiFun(..), HiExpr(..), HiValue(..), HiAction(..))
import Data.ByteString
import Data.Word (Word8)

type Parser = Parsec Void String


-- | Terms
-- pS -> pE | Infix pE
-- pE -> (pValue | ( pS ) | pDict) pE'
-- pDict -> { pDictValue, ... , pDictValue }
-- pDictValue -> pS : pS
-- pE' -> (! | !E' | .word)pE' | eps
-- pValue -> pBytes | pNumber | pFun | pBool | pText | pNull | pAction

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpr <* eof) ""

parserFun :: Parser HiFun
parserFun = choice [
  HiFunDiv <$ string "div",
  HiFunMul <$ string "mul",
  HiFunAdd <$ string "add",
  HiFunSub <$ string "sub",
  HiFunNotEquals <$ string "not-equals",
  HiFunNotLessThan <$ string "not-less-than",
  HiFunNotGreaterThan <$ string "not-greater-than",
  HiFunNot <$ string "not",
  HiFunAnd <$ string "and",
  HiFunOr <$ string "or",
  HiFunEquals <$ string "equals",
  HiFunLessThan <$ string "less-than",
  HiFunGreaterThan <$ string "greater-than",
  HiFunIf <$ string "if",
  HiFunLength <$ string "length",
  HiFunToUpper <$ string "to-upper",
  HiFunToLower <$ string "to-lower",
  HiFunReverse <$ string "reverse",
  HiFunTrim <$ string "trim",
  HiFunList <$ string "list",
  HiFunRange <$ string "range",
  HiFunFold <$ string "fold",
  HiFunPackBytes <$ string "pack-bytes",
  HiFunUnpackBytes <$ string "unpack-bytes",
  HiFunEncodeUtf8 <$ string "encode-utf8",
  HiFunDecodeUtf8 <$ string "decode-utf8",
  HiFunZip <$ string "zip",
  HiFunUnzip <$ string "unzip",
  HiFunSerialise <$ string "serialise",
  HiFunDeserialise <$ string "deserialise",
  HiFunKeys <$ string "keys",
  HiFunValues <$ string "values",
  HiFunInvert <$ string "invert",
  HiFunCount <$ string "count",
  HiFunRead <$ string "read",
  HiFunWrite <$ string "write",
  HiFunMkDir <$ string "mkdir",
  HiFunChDir <$ string "cd",
  HiFunParseTime <$ string "parse-time",
  HiFunRand <$ string "rand",
  HiFunEcho <$ string "echo" ]

parserBool :: Parser Bool
parserBool = choice [
  True  <$ string "true",
  False <$ string "false" ]

parserString :: Parser String
parserString = char '"' *> manyTill L.charLiteral (char '"')

parserValueScientific :: Parser Scientific
parserValueScientific = L.signed (pure ()) L.scientific

parserExprValueNumber :: Parser HiValue
parserExprValueNumber = fmap (HiValueNumber . toRational) parserValueScientific

parserExprValueFun :: Parser HiValue
parserExprValueFun = fmap HiValueFunction parserFun

parserExprValueBool :: Parser HiValue
parserExprValueBool = fmap HiValueBool parserBool

parserExprNull :: Parser HiValue
parserExprNull = HiValueNull <$ string "null"

parserExprValueText :: Parser HiValue
parserExprValueText = fmap (HiValueString . T.pack) parserString

parserExprValueList :: Parser HiExpr
parserExprValueList = do
  getArgs <- skipBothSides "[" "]" (pExpr `sepBy` char ',')
  pure (HiExprApply (HiExprValue (HiValueFunction HiFunList)) getArgs)

parserByteString :: Parser String
parserByteString = MP.count 2 hexDigitChar

parserExprValueBytes :: Parser HiValue
parserExprValueBytes = do
  getArgs <- skipBothSides "[#" "#]" (skipSpaces (parserByteString `sepEndBy` space1))
  let hexArgs = mapM readMaybeHex getArgs
  case hexArgs of
    Just bytes -> pure (HiValueBytes (Data.ByteString.pack bytes))
    Nothing    -> fail "Failed convert"

readMaybeHex :: String -> Maybe Word8
readMaybeHex s = case readMaybe ("0x" ++ s) of
  Just val | val >= 0 && val <= 255 -> Just (toEnum val)
  _                                 -> Nothing

parserExprValueAction :: Parser HiValue
parserExprValueAction = choice [
    HiValueAction HiActionCwd <$ string "cwd",
    HiValueAction HiActionNow <$ string "now" ]

parserExprValueNotList :: Parser HiValue
parserExprValueNotList = parserExprValueBytes
                      <|> parserExprValueNumber
                      <|> parserExprValueFun
                      <|> parserExprValueBool
                      <|> parserExprValueText
                      <|> parserExprNull
                      <|> parserExprValueAction

parserExprValue :: Parser HiExpr
parserExprValue = fmap HiExprValue (skipSpaces parserExprValueNotList) <|> skipSpaces parserExprValueList

parserBinArgs :: Parser [HiExpr]
parserBinArgs = skipBothSides "(" ")" (pExpr `sepBy` char ',')

pStandartArgsE' :: HiExpr -> Parser HiExpr
pStandartArgsE' func = do
  args <- skipSpaces parserBinArgs
  let applyFunction = HiExprApply func args
  pContE' applyFunction

pDotArgsE' :: HiExpr -> Parser HiExpr
pDotArgsE' func = do
  arg <- char '.' *> some letterChar
  let applyFunction = HiExprApply func [HiExprValue (HiValueString (T.pack arg))]
  pContE' applyFunction

pActionE' :: HiExpr -> Parser HiExpr
pActionE' action = do
  void (char '!' <* space)
  pContE' (HiExprRun action)

pContE' :: HiExpr -> Parser HiExpr
pContE' exprGet = pE' exprGet <|> pure exprGet

pE' :: HiExpr -> Parser HiExpr
pE' func = pStandartArgsE' func <|> pDotArgsE' func <|> pActionE' func

pV :: Parser HiExpr
pV = do
  pTerm <- skipSpaces parserExprValue
  pE' pTerm <|> pure pTerm

pBrackets :: Parser HiExpr
pBrackets = do
  pTerm <-skipSpaces (skipBothSides "(" ")" pExpr)
  pE' pTerm <|> pure pTerm

pDictValue :: Parser (HiExpr, HiExpr)
pDictValue = do
  pKey <- pExpr <* char ':'
  pValue <- pExpr
  pure (pKey, pValue)

pDict :: Parser HiExpr
pDict = do
  pTerm <- skipSpaces (skipBothSides "{" "}" (skipSpaces (pDictValue `sepBy` char ',')))
  pContE' (HiExprDict pTerm)

pE :: Parser HiExpr
pE = skipSpaces (pV <|> pBrackets <|> pDict)

pExpr :: Parser HiExpr
pExpr = skipSpaces (makeExprParser pE operatorTable)

skipSpaces :: Parser a -> Parser a
skipSpaces parser = space *> parser <* space

skipBothSides :: String -> String -> Parser a -> Parser a
skipBothSides skipStart skipEnd parser = string skipStart *> parser <* string skipEnd

pStringJustDiv :: Parser String
pStringJustDiv = skipSpaces (string "/" <* notFollowedBy (string "="))

getFunExpr :: HiFun -> HiExpr -> HiExpr -> HiExpr
getFunExpr fun left right = HiExprApply (HiExprValue (HiValueFunction fun)) [left, right]

binaryL :: HiFun -> String -> Operator Parser HiExpr
binaryL fun str = InfixL (getFunExpr fun <$ string str)

binaryR :: HiFun -> String -> Operator Parser HiExpr
binaryR fun str = InfixR (getFunExpr fun <$ string str)

binaryN :: HiFun -> String -> Operator Parser HiExpr
binaryN fun str = InfixN (getFunExpr fun <$ string str)

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
    [ InfixL (getFunExpr HiFunDiv <$ try pStringJustDiv),
      binaryL HiFunMul "*" ],
    [ binaryL HiFunAdd "+",
      binaryL HiFunSub "-" ],
    [ binaryN HiFunNotGreaterThan "<=",
      binaryN HiFunNotLessThan ">=",
      binaryN HiFunEquals "==",
      binaryN HiFunNotEquals "/=",
      binaryN HiFunLessThan "<",
      binaryN HiFunGreaterThan ">" ],
    [ binaryR HiFunAnd "&&" ],
    [ binaryR HiFunOr "||" ] ]
