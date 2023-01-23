{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module HW3.Evaluator (
    eval,
)
where

import Codec.Compression.Zlib (CompressParams (..), bestCompression, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Bits.Extras
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either (fromRight)
import qualified Data.Foldable
import qualified Data.Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator)
import Data.Sequence (
    Seq (Empty, (:<|), (:|>)),
    fromList,
    length,
    reverse,
    (><),
 )
import qualified Data.Sequence
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import GHC.Base (stimes)
import GHC.Real (numerator)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalHiExpr

evalHiExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalHiExpr (HiExprValue val) = return val
evalHiExpr (HiExprDict list) = HiValueDict . Data.Map.fromList <$> evalListPairs list
  where
    evalListPairs :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
    evalListPairs =
        mapM
            ( \(k, v) -> do
                k' <- evalHiExpr k
                v' <- evalHiExpr v
                return (k', v')
            )
evalHiExpr (HiExprApply fun list) = do
    res <- evalHiExpr fun
    evalExprList (getArity res) res list
evalHiExpr (HiExprRun forRun) = do
    res <- evalHiExpr forRun
    case res of
        HiValueAction action -> ExceptT $ Right <$> runAction action
        _ -> throwE HiErrorInvalidArgument

evalExprList :: HiMonad m => Arity -> HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalExprList UnaryOrBinary binary list@(_ : [_]) = evalExprList Binary binary list
evalExprList UnaryOrBinary single [t] = evalExprList Unary single [t]
evalExprList Vararg (HiValueFunction HiFunList) list = createSeq list Data.Sequence.Empty
evalExprList Binary (HiValueFunction HiFunAnd) (h : [t]) = (`evalBoolAnd` t) =<< evalHiExpr h
evalExprList Binary (HiValueFunction HiFunOr) (h : [t]) = (`evalBoolOr` t) =<< evalHiExpr h
evalExprList Unary single [t] = unaryOperation single =<< evalHiExpr t
evalExprList Binary binary (h : [t]) = do
    res <- evalHiExpr h
    binaryOperation binary res =<< evalHiExpr t
evalExprList Ternary triple (h : (m : [t])) = do
    res <- evalHiExpr h
    ternaryOperation triple res m t
evalExprList InvalidFunction _ _ = throwE HiErrorInvalidFunction
evalExprList _ _ _ = throwE HiErrorArityMismatch

evalBoolAnd :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
evalBoolAnd a b = case a of
    HiValueBool False -> return a
    HiValueNull -> return a
    _ -> evalHiExpr b

evalBoolOr :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
evalBoolOr a b = case a of
    HiValueBool False -> evalHiExpr b
    HiValueNull -> evalHiExpr b
    _ -> return a

createSeq :: HiMonad m => [HiExpr] -> Data.Sequence.Seq HiValue -> ExceptT HiError m HiValue
createSeq [] res = return $ HiValueList res
createSeq (h : t) res = createSeq t . (res Data.Sequence.:|>) =<< evalHiExpr h

evalUnary :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalUnary (HiValueFunction HiFunSerialise) s = return $ HiValueBytes $ toStrict $ serialise s
evalUnary (HiValueFunction fun) (HiValueBytes b) = operationBytes fun b
evalUnary (HiValueFunction fun) (HiValueString t) = operationString fun t
evalUnary (HiValueFunction fun) (HiValueDict d) = operationDict fun d
evalUnary (HiValueFunction fun) (HiValueList l) = operationList fun l
evalUnary (HiValueFunction HiFunNot) (HiValueBool a) = return $ HiValueBool $ not a
evalUnary _ _ = throwE HiErrorInvalidArgument

unaryOperation :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
unaryOperation (HiValueString t) (HiValueNumber n) = getByIndex t n
unaryOperation (HiValueList t) (HiValueNumber n) = getByIndex t n
unaryOperation (HiValueBytes t) (HiValueNumber n) = getByIndex t n
unaryOperation (HiValueDict mp) value = return $ fromMaybe HiValueNull $ Data.Map.lookup value mp
unaryOperation fun a = evalUnary fun a

binaryOperation :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
binaryOperation (HiValueString t) a b = slice t a b
binaryOperation (HiValueList t) a b = slice t a b
binaryOperation (HiValueBytes t) a b = slice t a b
binaryOperation (HiValueFunction HiFunWrite) str (HiValueString b) = binaryOperation (HiValueFunction HiFunWrite) str $ HiValueBytes $ encodeUtf8 b
binaryOperation (HiValueFunction HiFunWrite) (HiValueString t) (HiValueBytes b) = return $ HiValueAction $ HiActionWrite (Data.Text.unpack t) b
binaryOperation fun a b = calcBin fun a b

calcBin :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
calcBin (HiValueFunction HiFunAdd) (HiValueTime a) (HiValueNumber b) = return $ HiValueTime $ addUTCTime (fromRational b) a
calcBin (HiValueFunction HiFunSub) (HiValueTime a) (HiValueTime b) = returnHiValue $ diffUTCTime a b
calcBin (HiValueFunction HiFunFold) _ (HiValueList Data.Sequence.Empty) = return HiValueNull
calcBin (HiValueFunction HiFunFold) fun (HiValueList (h Data.Sequence.:<| t)) = case getArity fun of
    Binary -> foldList fun h t
    UnaryOrBinary -> foldList fun h t
    _ -> throwE HiErrorInvalidFunction
calcBin (HiValueFunction HiFunEquals) a b = return $ HiValueBool $ a == b
calcBin (HiValueFunction HiFunNotEquals) a b = return $ HiValueBool $ a /= b
calcBin (HiValueFunction HiFunLessThan) a b = return $ HiValueBool $ a < b
calcBin (HiValueFunction HiFunGreaterThan) a b = return $ HiValueBool $ a > b
calcBin (HiValueFunction HiFunNotGreaterThan) a b = return $ HiValueBool $ a <= b
calcBin (HiValueFunction HiFunNotLessThan) a b = return $ HiValueBool $ a >= b
calcBin (HiValueFunction HiFunAdd) (HiValueString a) (HiValueString b) = return $ HiValueString $ a <> b
calcBin (HiValueFunction HiFunAdd) (HiValueList a) (HiValueList b) = return $ HiValueList $ a <> b
calcBin (HiValueFunction HiFunAdd) (HiValueBytes a) (HiValueBytes b) = return $ HiValueBytes $ a <> b
calcBin (HiValueFunction HiFunDiv) (HiValueString a) (HiValueString b) = return $ HiValueString $ a <> Data.Text.pack "/" <> b
calcBin (HiValueFunction HiFunMul) (HiValueString a) (HiValueNumber b) = calcNumberTimes a b
calcBin (HiValueFunction HiFunMul) (HiValueList a) (HiValueNumber b) = calcNumberTimes a b
calcBin (HiValueFunction HiFunMul) (HiValueBytes a) (HiValueNumber b) = calcNumberTimes a b
calcBin (HiValueFunction fun) (HiValueNumber a) (HiValueNumber b) = operationNumber fun a b
calcBin _ _ _ = throwE HiErrorInvalidArgument

ternaryOperation :: HiMonad m => HiValue -> HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
ternaryOperation (HiValueFunction HiFunIf) (HiValueBool a) b c = if a then evalHiExpr b else evalHiExpr c
ternaryOperation _ _ _ _ = throwE HiErrorInvalidArgument

foldList :: HiMonad m => HiValue -> HiValue -> Data.Sequence.Seq HiValue -> ExceptT HiError m HiValue
foldList _ val Data.Sequence.Empty = return val
foldList fun val (h Data.Sequence.:<| t) = do
    ans <- evalExprList Binary fun [HiExprValue val, HiExprValue h]
    foldList fun ans t

data Arity
    = Unary
    | Binary
    | Ternary
    | UnaryOrBinary
    | Vararg
    | InvalidFunction

class Converter a where
    wrapToHiValue :: a -> HiValue
    returnHiValue :: HiMonad m => a -> ExceptT HiError m HiValue
    returnHiValue = return . wrapToHiValue

instance {-# OVERLAPPABLE #-} Real a => Converter a where
    wrapToHiValue = HiValueNumber . toRational

instance Converter Data.Text.Text where
    wrapToHiValue = HiValueString

instance Converter (Data.Sequence.Seq HiValue) where
    wrapToHiValue = HiValueList

instance Converter Data.ByteString.ByteString where
    wrapToHiValue = HiValueBytes

class SeqShell a where
    lengthF :: a -> Rational
    lengthF = toRational . lengthI
    lengthI :: a -> Int
    indexF :: a -> Int -> HiValue
    takeF :: Int -> a -> a
    dropF :: Int -> a -> a
    toListF :: a -> [HiValue]

instance SeqShell Data.Text.Text where
    lengthI = Data.Text.length
    indexF t ind = (HiValueString . Data.Text.pack) [Data.Text.index t ind]
    takeF = Data.Text.take
    dropF = Data.Text.drop
    toListF t = fmap (\c -> (HiValueString . Data.Text.pack) [c]) (Data.Text.unpack t)

instance SeqShell (Data.Sequence.Seq HiValue) where
    lengthI = Data.Sequence.length
    indexF = Data.Sequence.index
    takeF = Data.Sequence.take
    dropF = Data.Sequence.drop
    toListF = Data.Foldable.toList

instance SeqShell Data.ByteString.ByteString where
    lengthI = Data.ByteString.length
    indexF s ind = (HiValueNumber . toRational) $ Data.ByteString.index s ind
    takeF = Data.ByteString.take
    dropF = Data.ByteString.drop
    toListF b = fmap (HiValueNumber . toRational) (Data.ByteString.unpack b)

getArity :: HiValue -> Arity
getArity (HiValueFunction fun) = case fun of
    HiFunNot -> Unary
    HiFunLength -> Unary
    HiFunToUpper -> Unary
    HiFunToLower -> Unary
    HiFunReverse -> Unary
    HiFunTrim -> Unary
    HiFunPackBytes -> Unary
    HiFunUnpackBytes -> Unary
    HiFunZip -> Unary
    HiFunUnzip -> Unary
    HiFunSerialise -> Unary
    HiFunDeserialise -> Unary
    HiFunEncodeUtf8 -> Unary
    HiFunDecodeUtf8 -> Unary
    HiFunRead -> Unary
    HiFunMkDir -> Unary
    HiFunChDir -> Unary
    HiFunParseTime -> Unary
    HiFunEcho -> Unary
    HiFunCount -> Unary
    HiFunKeys -> Unary
    HiFunValues -> Unary
    HiFunInvert -> Unary
    HiFunDiv -> Binary
    HiFunMul -> Binary
    HiFunAdd -> Binary
    HiFunSub -> Binary
    HiFunAnd -> Binary
    HiFunOr -> Binary
    HiFunLessThan -> Binary
    HiFunGreaterThan -> Binary
    HiFunEquals -> Binary
    HiFunNotLessThan -> Binary
    HiFunNotGreaterThan -> Binary
    HiFunNotEquals -> Binary
    HiFunFold -> Binary
    HiFunRange -> Binary
    HiFunWrite -> Binary
    HiFunRand -> Binary
    HiFunIf -> Ternary
    HiFunList -> Vararg
getArity (HiValueString _) = UnaryOrBinary
getArity (HiValueList _) = UnaryOrBinary
getArity (HiValueBytes _) = UnaryOrBinary
getArity (HiValueDict _) = Unary
getArity _ = InvalidFunction

calcNumberTimes :: (HiMonad m, Semigroup a, Converter a) => a -> Rational -> ExceptT HiError m HiValue
calcNumberTimes a b = do
    ind <- getInt b
    if ind > 0
        then returnHiValue $ stimes ind a
        else throwE HiErrorInvalidArgument

slice :: (HiMonad m, SeqShell a, Converter a) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
slice t (HiValueNumber a) (HiValueNumber b) = doSlice t a b
slice t HiValueNull (HiValueNumber b) = doSlice t 0 b
slice t (HiValueNumber a) HiValueNull = doSlice t a $ lengthF t
slice t HiValueNull HiValueNull = doSlice t 0 $ lengthF t
slice _ _ _ = throwE HiErrorInvalidArgument

doSlice :: (HiMonad m, SeqShell a, Converter a) => a -> Rational -> Rational -> ExceptT HiError m HiValue
doSlice t a b = do
    leftI <- getInt a
    rightI <- getInt b
    let left = fixIndex t leftI
        right = fixIndex t rightI
    returnHiValue $ takeF (right - left) $ dropF left t

fixIndex :: (SeqShell a) => a -> Int -> Int
fixIndex t a
    | a < 0 = lengthI t + a
    | otherwise = a

getByIndex :: (SeqShell a, HiMonad m) => a -> Rational -> ExceptT HiError m HiValue
getByIndex t n = do
    ind <- getInt n
    if ind >= 0 && ind < lengthI t
        then return $ indexF t ind
        else return HiValueNull

getInt :: HiMonad m => Rational -> ExceptT HiError m Int
getInt rat = case (numerator rat, denominator rat) of
    (ind, 1)
        | toInteger (minBound :: Int) <= ind && ind <= toInteger (maxBound :: Int) -> return $ fromInteger ind
        | otherwise -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument

operationBytes :: HiMonad m => HiFun -> Data.ByteString.ByteString -> ExceptT HiError m HiValue
operationBytes = \case
    HiFunDeserialise -> return . fromRight HiValueNull . deserialiseOrFail . fromStrict
    HiFunUnpackBytes -> return . HiValueList . Data.Sequence.fromList . toListF
    HiFunUnzip -> return . HiValueBytes . toStrict . decompress . fromStrict
    HiFunDecodeUtf8 -> return . either (const HiValueNull) HiValueString . decodeUtf8'
    HiFunZip -> return . HiValueBytes . toStrict . compressWith (defaultCompressParams{compressLevel = bestCompression}) . fromStrict
    HiFunLength -> return . HiValueNumber . toRational . Data.ByteString.length
    HiFunReverse -> return . HiValueBytes . Data.ByteString.reverse
    HiFunCount -> return . countF . fmap (HiValueNumber . toRational) . Data.ByteString.unpack
    _ -> (\_ -> throwE HiErrorInvalidArgument)

operationDict :: HiMonad m => HiFun -> Data.Map.Map HiValue HiValue -> ExceptT HiError m HiValue
operationDict = \case
    HiFunInvert ->
        ( \dict ->
            (return . HiValueDict) $
                Data.Map.map HiValueList $
                    Data.Map.fromListWith (><) [(v, k :<| Empty) | (k, v) <- Data.Map.toList dict]
        )
    HiFunKeys -> return . HiValueList . Data.Sequence.fromList . Data.Map.keys
    HiFunValues -> return . HiValueList . Data.Sequence.fromList . Data.Map.elems
    _ -> (\_ -> throwE HiErrorInvalidArgument)

operationList :: HiMonad m => HiFun -> Seq HiValue -> ExceptT HiError m HiValue
operationList HiFunPackBytes t = createByteList t []
  where
    createByteList :: HiMonad m => Seq HiValue -> [Word8] -> ExceptT HiError m HiValue
    createByteList Data.Sequence.Empty res = (return . HiValueBytes . Data.ByteString.pack) res
    createByteList (h Data.Sequence.:|> (HiValueNumber rat)) res = do
        resValue <- getInt rat
        if 0 <= resValue && resValue <= 255
            then createByteList h (Data.Bits.Extras.w8 resValue : res)
            else throwE HiErrorInvalidArgument
    createByteList _ _ = throwE HiErrorInvalidArgument
operationList HiFunLength t = return $ HiValueNumber $ toRational $ Data.Sequence.length t
operationList HiFunReverse t = return $ HiValueList $ Data.Sequence.reverse t
operationList HiFunCount t = return $ countF (Data.Foldable.toList t)
operationList _ _ = throwE HiErrorInvalidArgument

operationNumber :: HiMonad m => HiFun -> Rational -> Rational -> ExceptT HiError m HiValue
operationNumber HiFunRand a b = do
    aVal <- getInt a
    bVal <- getInt b
    return $ HiValueAction $ HiActionRand aVal bVal
operationNumber HiFunRange a b = return $ HiValueList $ Data.Sequence.fromList $ Prelude.map HiValueNumber [a .. b]
operationNumber fun a b = case fun of
    HiFunAdd -> return $ HiValueNumber $ a + b
    HiFunSub -> return $ HiValueNumber $ a - b
    HiFunMul -> return $ HiValueNumber $ a * b
    HiFunDiv
        | b /= 0 -> return $ HiValueNumber $ a / b
        | otherwise -> throwE HiErrorDivideByZero
    _ -> throwE HiErrorInvalidArgument

operationString :: HiMonad m => HiFun -> Data.Text.Text -> ExceptT HiError m HiValue
operationString = \case
    HiFunToUpper -> return . HiValueString . Data.Text.toUpper
    HiFunToLower -> return . HiValueString . Data.Text.toLower
    HiFunTrim -> return . HiValueString . Data.Text.strip
    HiFunEncodeUtf8 -> return . HiValueBytes . encodeUtf8
    HiFunEcho -> return . HiValueAction . HiActionEcho
    HiFunParseTime -> return . maybe HiValueNull HiValueTime . readMaybe . Data.Text.unpack
    HiFunRead -> return . HiValueAction . HiActionRead . Data.Text.unpack
    HiFunMkDir -> return . HiValueAction . HiActionMkDir . Data.Text.unpack
    HiFunChDir -> return . HiValueAction . HiActionChDir . Data.Text.unpack
    HiFunLength -> return . HiValueNumber . toRational . Data.Text.length
    HiFunReverse -> return . HiValueString . Data.Text.reverse
    HiFunCount -> return . countF . (fmap (\c -> (HiValueString . Data.Text.pack) [c]) . Data.Text.unpack)
    _ -> (\_ -> throwE HiErrorInvalidArgument)

countF :: [HiValue] -> HiValue
countF t =
    HiValueDict $
        Data.Map.map HiValueNumber $
            Data.Map.fromListWith (+) pairs
  where
    pairs = [(k, 1) | k <- t]
