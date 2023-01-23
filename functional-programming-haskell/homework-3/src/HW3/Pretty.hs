{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Pretty (
    prettyValue,
)
where

import Data.ByteString (unpack)
import qualified Data.ByteString
import qualified Data.Foldable
import qualified Data.Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Time
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiValue (..))
import Numeric (showFFloat, showHex)
import Prettyprinter (Doc, Pretty (pretty), comma, hsep, punctuate)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
    HiValueBool flag -> prettyBool flag
    HiValueNumber number -> prettyNumber number
    HiValueFunction fun -> prettyFunction fun
    HiValueNull -> prettyNull
    HiValueString s -> prettyString s
    HiValueList l -> prettyList l
    HiValueBytes l -> prettyBytes l
    HiValueAction a -> prettyAction a
    HiValueTime time -> prettyTime time
    HiValueDict dict -> prettyDict dict

prettyBool :: Bool -> Doc AnsiStyle
prettyBool flag = pretty $ if flag then "true" else "false"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber x = pretty $ case (numerator x, denominator x) of
    (n, 1) -> show n
    (n, d) ->
        if divide (divide d 2) 5 == 1
            then case fromRationalRepetendUnlimited x of
                (s, Nothing) -> showFFloat Nothing (toRealFloat @Double s) ""
                (_, Just _) -> prettyFractional d (quotRem n d)
            else prettyFractional d (quotRem n d)
      where
        divide :: Integer -> Integer -> Integer
        divide num divider
            | num `mod` divider == 0 = divide (num `div` divider) divider
            | otherwise = num
        prettyFractional :: Integer -> (Integer, Integer) -> String
        prettyFractional divider (0, a) = show a ++ "/" ++ show divider
        prettyFractional divider (a, b) = show a ++ (if b > 0 then " + " else " - ") ++ prettyFractional divider (0, abs b)

prettyFunction :: Show a => a -> Doc AnsiStyle
prettyFunction fun = pretty $ show fun

prettyNull :: Doc AnsiStyle
prettyNull = pretty "null"

prettyString :: Data.Text.Text -> Doc AnsiStyle
prettyString = pretty . show

prettyList :: Data.Sequence.Seq HiValue -> Doc AnsiStyle
prettyList l =
    hsep
        ( pretty "["
            : punctuate comma (Prelude.map prettyValue (Data.Foldable.toList l))
        )
        <> pretty " ]"

prettyBytes :: Data.ByteString.ByteString -> Doc AnsiStyle
prettyBytes =
    pretty
        . ( \bytes ->
                "[# "
                    <> Data.Foldable.foldr'
                        ((<>) . prettyHex)
                        "#]"
                        (Data.ByteString.unpack bytes)
          )
  where
    prettyHex :: Word8 -> String
    prettyHex byte
        | byte < 16 = "0" ++ showHex byte " "
        | otherwise = showHex byte " "

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
    HiActionRead path -> pretty $ "read(" <> "\"" <> path <> "\"" <> ")"
    HiActionWrite path bytes ->
        pretty "write("
            <> pretty ("\"" <> path <> "\"")
            <> pretty ", "
            <> prettyBytes bytes
            <> pretty ")"
    HiActionMkDir path -> pretty $ "mkdir(" <> "\"" <> path <> "\"" <> ")"
    HiActionChDir path -> pretty $ "cd(" <> "\"" <> path <> "\"" <> ")"
    HiActionCwd -> pretty "cwd"
    HiActionNow -> pretty "now"
    HiActionRand a b -> pretty $ "rand(" <> show a <> ", " <> show b <> ")"
    HiActionEcho t -> pretty $ "echo(" <> show t <> ")"

prettyTime :: Data.Time.UTCTime -> Doc AnsiStyle
prettyTime time = pretty $ "parse-time(\"" ++ show time ++ "\")"

prettyDict :: Data.Map.Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict =
    hsep
        ( pretty "{"
            : punctuate
                comma
                (Prelude.map (\(a, b) -> prettyValue a <> pretty ": " <> prettyValue b) $ Data.Map.toList dict)
        )
        <> pretty " }"
