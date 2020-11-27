{-# LANGUAGE DeriveGeneric, DerivingVia, RecordWildCards #-}

-- | Module that handles ligo binary execution.
module Cli.Impl
  ( LigoBinaryCallError(..)
  , callLigo
  , getLigoDefinitions
  , parseLigoDefinitions
  , parseLigoOutput
  , getLigoDefinitionsFrom
  ) where

import Control.Exception.Safe (Exception (..), SomeException, catchAny, throwIO, try)
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader
import Data.Aeson (eitherDecodeStrict')
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as S8L
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Duplo.Pretty (PP (PP), Pretty (..), text, (<+>), (<.>))
import System.Exit (ExitCode (..))
import System.Process

import Cli.Json
import Cli.Types
import Extension (Lang (..), getExt)
import Log (i)
import qualified Log
import ParseTree (Source (..), srcToText)

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

data LigoBinaryCallError
  = -- | ligo call unexpectedly failed (returned non-zero exit code).
    -- The error contains the error code, stdout and stderr contents.
    UnexpectedClientFailure
      Int -- ^ Exit code
      Text -- ^ stdout
      Text -- ^ stderr

  | -- | Catch expected ligo failure to be able to restore from it.
    ExpectedClientFailure
      Text -- ^ stdout
      Text -- ^ stderr

  | -- | Expected ligo failure decoded from its JSON output.
    DecodedExpectedClientFailure
      LigoError -- ^ decoded JSON stderr

  -- Below are the errors which may fail due to some changes in ligo compiller.

  --   -- | Ligo compiller produced a type which we consider is malformed
  --   MalformedType
  --     Text
  | -- | Parse error occured during ligo output JSON decoding.
    DefinitionParseError
      Text
  | -- | Parse error occured during ligo stderr JSON decoding.
    LigoErrorNodeParseError
      Text
  deriving (Show) via PP LigoBinaryCallError

instance Exception LigoBinaryCallError where
  displayException = show . pp

instance Pretty LigoBinaryCallError where
  pp = \case
    UnexpectedClientFailure errCode output errOutput ->
      "ligo binary unexpectedly failed with error code" <+> pp errCode
        <+> ".\nStdout:\n" <.> pp output <.> "\nStderr:\n" <.> pp errOutput
    ExpectedClientFailure output errOutput ->
      "ligo binary failed as expected with\nStdout:\n" <.> pp output
      <.> "\nStderr:\n" <.> pp errOutput
    DecodedExpectedClientFailure err ->
      "ligo binary produced expected error which we successfully decoded as:\n" <.> text (show err)
    LigoErrorNodeParseError err ->
      "ligo binary produced error JSON which we consider malformed:\n" <.> pp err <.> "[end]"
    DefinitionParseError err ->
      "ligo binary produced output which we consider malformed:\n" <.> pp err

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

-- | Call ligo binary and return stdin and stderr accordingly.
callLigo
  :: HasLigoClient m => [String] -> Source -> m (Text, Text)
callLigo args con = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    raw <- srcToText con
    (ec, lo, le) <- readProcessWithExitCode' _lceClientPath args (unpack raw)
    unless (ec == ExitSuccess && le == mempty) $ do -- TODO: separate JSON errors and other ones
      throwM $ ExpectedClientFailure (pack lo) (pack le)
    unless (le == mempty) $ do
      throwM $ UnexpectedClientFailure 0 (pack lo) (pack le)
    Log.debug "LIGO" [i|Successfully exited with stdout:\n#{lo}\nand stderr:\n#{le}|]
    return (pack lo, pack le)

-- | Call ligo binary and pass raw contract to its stdin and return
-- stdin and stderr accordingly.
-- callLigoWith
--   :: HasLigoClient m => [String] -> Source -> m (Text, Text)
-- callLigoWith args con = do
--   env@LigoClientEnv {..} <- getLigoClientEnv
--   liftIO $ do
--     Log.debug "LIGO" [i|Running ligo on #{env} with #{args}|]
--     (Just ligoIn, Just ligoOut, Just ligoErr, ligoProc) <-
--       createProcess (proc _lceClientPath args)
--         { std_out = CreatePipe
--         , std_in  = CreatePipe
--         , std_err = CreatePipe
--         }
--     raw <- srcToBytestring con
--     S.hPut ligoIn raw
--     res <- S.hGetContents ligoOut
--     le <- S.hGetContents ligoErr
--     ec <- waitForProcess ligoProc
--     unless (ec == ExitSuccess) $ do
--       throwM $ ExpectedClientFailure (decodeUtf8 res) (decodeUtf8 le)
--     unless (le == mempty) $ do
--       throwM $ UnexpectedClientFailure 0 (decodeUtf8 res) (decodeUtf8 le)
--     Log.debug "LIGO" [i|Successfully exited with stdout:\n#{S8.unpack res}\nand stderr:\n#{S8.unpack le}|]
--     return (decodeUtf8 res, decodeUtf8 le)

-- | Variant of @readProcessWithExitCode@ that prints a better error in case of
-- an exception in the inner @readProcessWithExitCode@ call.
readProcessWithExitCode'
  :: FilePath
  -> [String]
  -> String
  -> IO (ExitCode, String, String)
readProcessWithExitCode' fp args inp =
    readProcessWithExitCode fp args inp `catchAny` handler
  where
    handler :: SomeException -> IO (ExitCode, String, String)
    handler e = do
      Log.err "CLI" errorMsg
      throwIO e

    errorMsg =
      mconcat
        [ "ERROR!! There was an error in executing `"
        , show fp
        , "` program. Is the executable available in PATH ?"
        ]

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Parse from output file

-- | Parse ligo definitions from ligo output file generated by
-- ```
-- ligo get-scope `cat ${contract_path}` --format=json --with-types
-- ```
-- and return a hashmap of scope name and its values.
parseLigoDefinitions
  :: HasLigoClient m
  => FilePath
  -> m LigoDefinitions
parseLigoDefinitions contractPath = do
  output <- liftIO $ S8L.readFile contractPath
  case eitherDecodeStrict' . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ DefinitionParseError (pack err)
    Right definitions -> return definitions

-- | Helper function used for parsing parts of ligo JSON output.
parseLigoOutput
  :: forall a . FromJSON a => FilePath -> IO a
parseLigoOutput contractPath = do
  output <- S8L.readFile contractPath
  case eitherDecodeStrict' @a . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ DefinitionParseError (pack err)
    Right definitions -> return definitions

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get ligo definitions from a contract by calling ligo binary.
getLigoDefinitionsFrom
  :: HasLigoClient m
  => FilePath
  -> m (LigoDefinitions, Text)
getLigoDefinitionsFrom contractPath = do
  contents <- liftIO $ S8L.readFile contractPath
  getLigoDefinitions $ ByteString contractPath (S8L.toStrict contents)

-- | Get ligo definitions from raw contract.
getLigoDefinitions
  :: HasLigoClient m
  => Source
  -> m (LigoDefinitions, Text)
getLigoDefinitions contract = do
  Log.debug "LIGO.PARSE" [i|parsing the following contract:\n #{contract}|]
  ext <- getExt (srcPath contract)
  let
    syntax = case ext of
      Reason -> "reasonligo"
      Pascal -> "pascaligo"
      Caml   -> "cameligo"
  mbOut <- try $
    callLigo ["get-scope", "--format=json", "--with-types", "--syntax=" <> syntax, "/dev/stdin"] contract
  case mbOut of
    Right (output, errs) -> do
      Log.debug "LIGO.PARSE" [i|Successfully called ligo with #{output}|]
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do
          Log.debug "LIGO.PARSE" [i|Unable to parse ligo definitions with: #{err}|]
          throwM $ DefinitionParseError (pack err)
        Right definitions -> return (definitions, errs)

    -- A middleware for processing `ExpectedClientFailure` error needed to pass it multiple levels up
    -- allowing us from restoring from expected ligo errors.
    Left (ExpectedClientFailure _ ligoStdErr) -> do
      -- otherwise call ligo with `compile-contract` to extract more readable error message
      Log.debug "LIGO.PARSE" [i|decoding ligo error|]
      case eitherDecodeStrict' @LigoError . encodeUtf8 $ ligoStdErr of
        Left err -> do
          Log.debug "LIGO.PARSE" [i|ligo error decoding failure #{err}|]
          throwM $ LigoErrorNodeParseError (pack err)
        Right decodedError -> do
          Log.debug "LIGO.PARSE" [i|ligo error decoding successfull with:\n#{decodedError}|]
          throwM $ DecodedExpectedClientFailure decodedError

    -- All other errors remain untouched
    Left err -> throwM err
