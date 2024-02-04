{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char

import HW4.Types
import HW4.T1 (ExceptState(..))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Return result by parser of given string
runP :: Parser a -> String -> Except ParseError a
runP (P parser) parseLine = case runES parser (0, parseLine) of
  Success (a :# _) -> Success a
  Error e -> Error e

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
-- | Parser which return if string empty then return error else return first char in string
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | Parser which return always error
parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p) (P q) = P $ ES $ \(pos, s) -> case runES p (pos, s) of
    Success val -> Success val
    Error _ -> runES q (pos, s)

-- No metohds
instance MonadPlus Parser

-- | Parser which return if string not empty then return error
pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> case s of
    [] -> Success (() :# (pos, s))
    _ -> Error (ErrorAtPos pos)

-- | Return result like expression by parsing of given string
--
-- LL(1) context free grammar view like:
--
-- pExpr -> pExprE
--
-- pExprE -> pExprT pExprE'
--
-- pExprE' -> (+|-) pExprT pExprE' | eps
--
-- pExprT -> pExprF pExprT'
--
-- pExprT' -> (*|/) pExprF pExprT' | eps
--
-- pExprF -> ( pExprE ) | pDouble
--
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr

pExpr :: Parser Expr
pExpr = pSpaceIs *> pExprE <* pSpaceIs <* pEof

pExprTerm :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
pExprTerm pTerm pTerm' = do
  exprTerm <- pTerm
  pTerm' exprTerm <|> pure exprTerm

pExprE :: Parser Expr
pExprE = pExprTerm pExprT pExprE'

pExprT :: Parser Expr
pExprT = pExprTerm pExprF pExprT'

pExprBinOp :: Parser Expr
           -> (Expr -> Parser Expr)
           -> (Char -> Expr -> Expr -> Parser Expr)
           -> Expr
           -> Parser Expr
pExprBinOp pTerm pTerm' setExpr left = do
    operation <- pSpaceIs *> pChar <* pSpaceIs
    right <- pTerm
    getExpr <- setExpr operation left right
    pTerm' getExpr <|> pure getExpr

getPrimExprByOpGet :: Char -> Expr -> Expr -> Maybe (Prim Expr)
getPrimExprByOpGet op left right = case op of
    '+' -> Just (Add left right)
    '-' -> Just (Sub left right)
    '*' -> Just (Mul left right)
    '/' -> Just (Div left right)
    _ -> Nothing

getExprByOpAllow :: [Char] -> Char -> Expr -> Expr -> Parser Expr
getExprByOpAllow opAllow op left right =
  if op `elem` opAllow
    then case getPrimExprByOpGet op left right of
      Just val -> pure (Op val)
      Nothing -> empty
  else empty

pExprE' :: Expr -> Parser Expr
pExprE' = pExprBinOp pExprT pExprE' (getExprByOpAllow "+-")

pExprT' :: Expr -> Parser Expr
pExprT' = pExprBinOp pExprF pExprT' (getExprByOpAllow "*/")

pExprF :: Parser Expr
pExprF = pCharIs '(' *> pSpaceIs *> pExprE <* pSpaceIs <* pCharIs ')' <|> pDouble

pCharIs :: Char -> Parser Char
pCharIs filterEquals = mfilter (filterEquals ==) pChar

pSpaceIs :: Parser [Char]
pSpaceIs = many (pCharIs ' ')

converterStrToInt :: String -> Double
converterStrToInt s = fromIntegral (foldl (\ value symbol -> value * 10 + digitToInt symbol) 0 s)

pStrInt :: Parser String
pStrInt = some (mfilter Data.Char.isDigit pChar)

pDouble :: Parser Expr
pDouble = do
  integralStrPart <- pStrInt
  fractionalStrPart <- pCharIs '.' *> pStrInt <|> pure "0"
  let integralPart = converterStrToInt integralStrPart
  let fractionalPart = converterStrToInt fractionalStrPart / (10 ** fromIntegral (length fractionalStrPart))
  pure (Val (integralPart + fractionalPart))