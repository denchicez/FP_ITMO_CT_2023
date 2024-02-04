{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , HiAction (..)
  , HiMonad (..)
  ) where
import Data.Text
import Data.Sequence
import Data.ByteString
import Codec.Serialise
import Data.Map
import GHC.Generics
import Data.Time.Clock

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Generic, Show, Eq, Ord)
instance Serialise HiAction

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  deriving (Generic, Show, Eq, Ord)
instance Serialise HiFun


data HiValue =
    HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueDict (Map HiValue HiValue)
  | HiValueAction HiAction
  | HiValueTime UTCTime
  deriving (Generic, Show, Eq, Ord)
instance Serialise HiValue

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)]
  | HiExprRun HiExpr

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue