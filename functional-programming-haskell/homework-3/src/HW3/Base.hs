{-# LANGUAGE DeriveGeneric #-}

module HW3.Base (
    HiAction (..),
    HiError (..),
    HiExpr (..),
    HiFun (..),
    HiMonad (..),
    HiValue (..),
)
where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiError
    = HiErrorInvalidArgument
    | HiErrorInvalidFunction
    | HiErrorArityMismatch
    | HiErrorDivideByZero
    deriving (Eq, Ord, Show)

data HiExpr
    = HiExprValue HiValue
    | HiExprApply HiExpr [HiExpr]
    | HiExprRun HiExpr
    | HiExprDict [(HiExpr, HiExpr)]
    deriving (Eq, Ord, Show)

data HiAction
    = HiActionRead FilePath
    | HiActionWrite FilePath ByteString
    | HiActionMkDir FilePath
    | HiActionChDir FilePath
    | HiActionCwd
    | HiActionNow
    | HiActionRand Int Int
    | HiActionEcho Text
    deriving (Eq, Ord, Show, Generic)

instance Serialise HiAction

data HiValue
    = HiValueBool Bool
    | HiValueNumber Rational
    | HiValueFunction HiFun
    | HiValueNull
    | HiValueString Text
    | HiValueList (Seq HiValue)
    | HiValueBytes ByteString
    | HiValueAction HiAction
    | HiValueTime UTCTime
    | HiValueDict (Map HiValue HiValue)
    deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiFun
    = HiFunDiv
    | HiFunMul
    | HiFunAdd
    | HiFunSub
    | HiFunAnd
    | HiFunOr
    | HiFunLessThan
    | HiFunGreaterThan
    | HiFunEquals
    | HiFunNotLessThan
    | HiFunNotGreaterThan
    | HiFunNotEquals
    | HiFunNot
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
    | HiFunRead
    | HiFunWrite
    | HiFunMkDir
    | HiFunChDir
    | HiFunParseTime
    | HiFunRand
    | HiFunEcho
    | HiFunCount
    | HiFunKeys
    | HiFunValues
    | HiFunInvert
    deriving (Eq, Ord, Enum, Generic)

instance Serialise HiFun

instance Show HiFun where
    show fun = case fun of
        HiFunDiv -> "div"
        HiFunMul -> "mul"
        HiFunAdd -> "add"
        HiFunSub -> "sub"
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
        HiFunZip -> "zip"
        HiFunUnzip -> "unzip"
        HiFunEncodeUtf8 -> "encode-utf8"
        HiFunDecodeUtf8 -> "decode-utf8"
        HiFunSerialise -> "serialise"
        HiFunDeserialise -> "deserialise"
        HiFunRead -> "read"
        HiFunWrite -> "write"
        HiFunMkDir -> "mkdir"
        HiFunChDir -> "cd"
        HiFunParseTime -> "parse-time"
        HiFunRand -> "rand"
        HiFunEcho -> "echo"
        HiFunCount -> "count"
        HiFunKeys -> "keys"
        HiFunValues -> "values"
        HiFunInvert -> "invert"

class Monad m => HiMonad m where
    runAction :: HiAction -> m HiValue
