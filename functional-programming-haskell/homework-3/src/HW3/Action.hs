{-# LANGUAGE LambdaCase #-}

module HW3.Action (
    HIO (..),
    HiPermission (..),
    PermissionException (..),
)
where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Exception (Exception, throwIO)
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Sequence
import Data.Set (Set)
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.POSIX (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random.Stateful (getStdRandom, uniformR)

data HiPermission
    = AllowRead
    | AllowWrite
    | AllowTime
    deriving (Ord, Enum, Eq, Show, Bounded)

newtype PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Functor HIO where
    fmap f a = HIO (fmap f . runHIO a)

instance Applicative HIO where
    pure a = HIO $ \_ -> pure a
    (<*>) = Control.Monad.ap

instance Monad HIO where
    m >>= f = HIO $ \env -> do
        v <- runHIO m env
        runHIO (f v) env

instance HiMonad HIO where
    runAction = \case
        HiActionRead path ->
            runOrThrow
                AllowRead
                ( do
                    let toHiValueList = (HiValueList . Data.Sequence.fromList) . fmap (HiValueString . Data.Text.pack)
                        readBytes bytes = either (\_ -> HiValueBytes bytes) HiValueString $ decodeUtf8' bytes
                    (readBytes <$> Data.ByteString.readFile path)
                        <|> (toHiValueList <$> listDirectory path)
                )
        HiActionWrite path bytes ->
            runOrThrow
                AllowWrite
                ( do
                    Data.ByteString.writeFile path bytes
                    pure HiValueNull
                )
        HiActionChDir path ->
            runOrThrow
                AllowRead
                ( do
                    setCurrentDirectory path
                    pure HiValueNull
                )
        HiActionMkDir path ->
            runOrThrow
                AllowWrite
                ( do
                    _ <- optional $ createDirectory path
                    pure HiValueNull
                )
        HiActionCwd -> runOrThrow AllowRead (HiValueString . Data.Text.pack <$> getCurrentDirectory)
        HiActionNow -> runOrThrow AllowTime (HiValueTime <$> getCurrentTime)
        HiActionRand a b -> HIO (\_ -> HiValueNumber . toRational <$> getStdRandom (uniformR (a, b)))
        HiActionEcho text ->
            runOrThrow
                AllowWrite
                ( do
                    putStrLn $ Data.Text.unpack text
                    pure HiValueNull
                )

runOrThrow :: HiPermission -> IO a -> HIO a
runOrThrow action func = HIO $ \env ->
    if action `elem` env
        then func
        else throwIO $ PermissionRequired action
