{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

module HW5.Action ( HiPermission(..), PermissionException(..), HIO(..) ) where

import HW5.Base
import Data.Set (Set)
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader
import System.Directory (createDirectory, getCurrentDirectory, setCurrentDirectory,
                        listDirectory, doesFileExist, doesDirectoryExist)
import Data.Text (unpack, pack)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString (readFile, writeFile)
import Data.Sequence (fromList)
import Control.Monad (unless)
import Data.Time.Clock (getCurrentTime)
import System.Random (uniformR, getStdRandom)

-- | Permission to read, write, time
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

-- | Exception if your permission don't allow action permission
data PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad)
  via (ReaderT (Set HiPermission) IO)


instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue

  -- | Get current path
  runAction HiActionCwd = HIO (\permissions -> do
      unless (AllowRead `elem` permissions) (throwIO (PermissionRequired AllowRead))
      HiValueString . pack <$> getCurrentDirectory )

  -- | Change directory path
  runAction (HiActionChDir path) = HIO (\permissions -> do
      unless (AllowRead `elem` permissions) (throwIO (PermissionRequired AllowRead))
      setCurrentDirectory path
      pure HiValueNull )

  -- | Read file or get files in directory
  runAction (HiActionRead path) = HIO (\permissions -> do
      unless (AllowRead `elem` permissions) (throwIO (PermissionRequired AllowRead))
      fileExist <- doesFileExist path
      (if fileExist then (do
        contentFile <- Data.ByteString.readFile path
        case decodeUtf8' contentFile of
          Left _ -> pure (HiValueBytes contentFile)
          Right str -> pure (HiValueString str)) else (do
        dirExist <- doesDirectoryExist path
        (if dirExist then (do
          directory_paths <- listDirectory path
          let listDirPaths = fmap (HiValueString . pack) directory_paths
          pure (HiValueList (fromList listDirPaths))) else pure HiValueNull))) )

  -- | Write content file
  runAction (HiActionWrite path content) = HIO (\permissions -> do
      unless (AllowWrite `elem` permissions) (throwIO (PermissionRequired AllowWrite))
      Data.ByteString.writeFile path content
      pure HiValueNull)

  -- | Create directory
  runAction (HiActionMkDir path) = HIO (\permissions -> do
      unless (AllowWrite `elem` permissions) (throwIO (PermissionRequired AllowWrite))
      createDirectory path
      pure HiValueNull)

  -- | Get current time
  runAction HiActionNow = HIO (\permissions -> do
        unless (AllowTime `elem` permissions) (throwIO (PermissionRequired AllowTime))
        HiValueTime <$> getCurrentTime)

  -- | Rand in [left, right] number
  runAction (HiActionRand left right) = HIO (\_ -> do
        rollDice <- getStdRandom (uniformR (left, right))
        pure (HiValueNumber (toRational rollDice)))

  -- | Echo message in console
  runAction (HiActionEcho message) = HIO (\permissions -> do
        unless (AllowWrite `elem` permissions) (throwIO (PermissionRequired AllowWrite))
        putStrLn (unpack message)
        pure HiValueNull)