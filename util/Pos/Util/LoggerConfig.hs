{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.LoggerConfig
       ( LoggerConfig (..)
       , RotationParameters (..)
       , LogHandler (..)
       , LoggerTree (..)
       , LogSafety (..)
       , BackendKind (..)
       , defaultTestConfiguration
       -- * access
       , lcLoggerTree
       , lcRotation
       , lcBasePath
       , ltHandlers
       , ltMinSeverity
       , rpKeepFiles
       , rpLogLimit
       , lhBackend
       , lhName
       , lhFpath
       , lhMinSeverity
       , lhSafety
       -- * functions
       , parseLoggerConfig
       , retrieveLogFiles
       ) where

import           Data.Yaml as Y
import           GHC.Generics
import           Universum

import           Control.Lens (each, makeLenses)
import qualified Data.Text as T

import           System.FilePath (normalise)

import           Pos.Util.Log.Severity


-- | @'BackendKind@ defines the available backends
data BackendKind = FileTextBE
                 | FileJsonBE
                 | StdoutBE
                 | DevNullBE
                 deriving (Eq, Generic, Show)
instance ToJSON BackendKind
instance FromJSON BackendKind where


-- | @'RotationParameters'@ one of the two categories  used in the
--   logging config, specifying the log rotation parameters
data RotationParameters = RotationParameters
    { _rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , _rpKeepFiles :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show)

instance ToJSON RotationParameters
instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        _rpLogLimit  <- o .: "logLimit"
        _rpKeepFiles <- o .: "keepFiles"
        return RotationParameters{..}

makeLenses ''RotationParameters

data LogSafety where
    PublicLog  :: LogSafety
    PrivateLog :: LogSafety
        deriving (Generic,Show)

deriving instance ToJSON LogSafety
deriving instance FromJSON LogSafety

-- | @'LogHandler'@ describes the output handler (file, stdout, ..)
--
data LogHandler = LogHandler
    { _lhName        :: !T.Text
      -- ^ name of the handler
    , _lhFpath       :: !(Maybe FilePath)
      -- ^ file path
    , _lhSafety      :: !(Maybe LogSafety)
      -- ^ file will be public or private
    , _lhBackend     :: !BackendKind
      -- ^ describes the backend (scribe for katip) to be loaded
    , _lhMinSeverity :: !(Maybe Severity)
      -- ^ the minimum severity to be logged
    } deriving (Generic,Show)

instance ToJSON LogHandler
instance FromJSON LogHandler where
    parseJSON = withObject "log handler" $ \o -> do
        (_lhName :: T.Text) <- o .: "name"
        (_lhFpath :: Maybe FilePath) <- fmap normalise <$> o .:? "filepath"
        (_lhSafety :: Maybe LogSafety) <- o .:? "logsafety"
        (_lhBackend :: BackendKind ) <- o .: "backend"
        (_lhMinSeverity :: Maybe Severity) <- o .:? "severity"
        pure LogHandler{..}

makeLenses ''LogHandler

-- | @'LoggerTree'@ contains the actual logging configuration,
--    'Severity' and 'LogHandler'
data LoggerTree = LoggerTree
    { _ltMinSeverity :: !Severity
    , _ltHandlers    :: ![LogHandler]
    } deriving (Generic, Show)

instance ToJSON LoggerTree
instance FromJSON LoggerTree where
    parseJSON = withObject "logger tree" $ \o -> do
        (singleFile :: Maybe FilePath) <- fmap normalise <$> o .:? "file"
        (manyFiles :: [FilePath]) <- map normalise <$> (o .:? "files" .!= [])
        handlers <- o .:? "handlers" .!= []
        let fileHandlers =
              map (\fp -> LogHandler { _lhName=T.pack fp, _lhFpath=Just fp
                                     , _lhBackend=FileTextBE, _lhMinSeverity=Just Debug
                                     --FIXME to have a condition that chooses if a file is
                                     -- public or private
                                     , _lhSafety=Just PublicLog }) $
                maybeToList singleFile ++ manyFiles
        let _ltHandlers = fileHandlers <> handlers
        (_ltMinSeverity :: Severity) <- o .: "severity" .!= Debug
        return LoggerTree{..}

instance Semigroup LoggerTree
instance Monoid LoggerTree where
    mempty = LoggerTree { _ltMinSeverity = Debug
                   , _ltHandlers = [LogHandler { _lhName="node", _lhFpath=Just "node.log"
                                               , _lhBackend=FileTextBE, _lhMinSeverity=Just Debug
                                               , _lhSafety=Just PublicLog}]
                   }
        -- ^ default value
    mappend = (<>)

makeLenses ''LoggerTree

-- | @'LoggerConfig'@ is the top level configuration datatype
data LoggerConfig = LoggerConfig
    { _lcRotation   :: !(Maybe RotationParameters)
    , _lcLoggerTree :: !LoggerTree
    , _lcBasePath   :: !(Maybe FilePath)
    } deriving (Generic, Show)

instance ToJSON LoggerConfig
instance FromJSON LoggerConfig where
    parseJSON = withObject "config " $ \o -> do
        _lcRotation <- o .:? "rotation"
        _lcLoggerTree <- o .: "loggerTree"
        _lcBasePath <- o .:? "logdir"
        return LoggerConfig{..}

instance Semigroup LoggerConfig
instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Just RotationParameters {
                                            _rpLogLimit = 10 * 1024 * 1024,
                                            _rpKeepFiles = 10 }
                     , _lcLoggerTree = mempty
                     , _lcBasePath = Nothing
                     }
        -- ^ default value
    mappend = (<>)

makeLenses ''LoggerConfig


-- | 'parseLoggerConfig' parses a file for the standard logging
--    configuration. Exceptions about opening the file (non existent/permissions)
--    are not handled here. Currently porting log-warper's definition
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig lcPath =
    liftIO $ join $ either throwM return <$> Y.decodeFileEither lcPath


-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle. Filepath inside does __not__
-- contain the common logger config prefix.
-- (this function was in infra/Pos/Reporting/Methods.hs)
retrieveLogFiles :: LoggerConfig -> [(Text, FilePath)]
retrieveLogFiles lc =
    map (\LogHandler{..} -> (_lhName, fromMaybe "<unk>" _lhFpath)) $
      filter (\LogHandler{..} -> isJust _lhFpath) lhs
    where
        lhs = lc ^. lcLoggerTree ^. ltHandlers ^.. each


-- | @LoggerConfig@ used in testing
-- no output and minimum Debug severity
defaultTestConfiguration :: Severity -> LoggerConfig
defaultTestConfiguration minSeverity =
    let _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltHandlers = [ LogHandler {
                _lhBackend = DevNullBE,
                _lhName = "devnull",
                _lhFpath = Nothing,
                _lhSafety = Just PrivateLog,
                _lhMinSeverity = Just minSeverity } ]
          }
    in
    LoggerConfig{..}
