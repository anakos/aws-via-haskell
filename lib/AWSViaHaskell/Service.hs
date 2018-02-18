{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AWSViaHaskell.Service
    ( AWSConfig
    , Endpoint(..)
    , Logging(..)
    , ServiceClass(..)
    , Session
    , SessionClass(..)
    , awscCredentials
    , awscEndpoint
    , awscLogging
    , awsConfig
    , connect
    , withAWS
    ) where

import           AWSViaHaskell.Classes
import           AWSViaHaskell.Types
import           Control.Lens ((<&>), Lens', set)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , reconfigure
                    , runAWST
                    , within
                    )
import           Control.Monad.Trans.Resource
                    ( MonadBaseControl
                    , ResourceT
                    )

import           Network.AWS
                    ( Credentials(..)
                    , Env
                    , LogLevel(..)
                    , Logger
                    , Region(..)
                    , envLogger
                    , newEnv
                    , newLogger
                    , runResourceT
                    , setEndpoint
                    )
import Protolude hiding ((<&>))
import           System.IO (stdout)

type HostName = ByteString

type Port = Int

data Logging = LoggingEnabled | LoggingDisabled

data Endpoint = AWSRegion Region | Local HostName Port

data AWSConfig = AWSConfig
    { _awscEndpoint    :: Endpoint
    , _awscLogging     :: Logging
    , _awscCredentials :: Credentials
    }

awscCredentials :: Lens' AWSConfig Credentials
awscCredentials f cfg@AWSConfig {..} =
  (\c -> cfg { _awscCredentials = c }) <$> f _awscCredentials

awscEndpoint :: Lens' AWSConfig Endpoint
awscEndpoint f cfg@AWSConfig {..}    =
  (\e -> cfg { _awscEndpoint = e }) <$> f _awscEndpoint

awscLogging :: Lens' AWSConfig Logging
awscLogging f cfg@AWSConfig {..}     =
  (\l -> cfg { _awscLogging = l }) <$> f _awscLogging

awsConfig :: Endpoint -> AWSConfig
awsConfig endpoint = AWSConfig endpoint LoggingDisabled Discover

connect ::
  forall a . ServiceClass a
  => AWSConfig
  -> a
  -> IO (TypedSession a)
connect AWSConfig {..} service = do
  e          <- mkEnv _awscLogging _awscCredentials
  let (r, s) = regionService _awscEndpoint (rawService service)
  session'   <- return $ Session e r s
  return $ wrappedSession @a session'
  where
    regionService (AWSRegion region) s    = (region, s)
    regionService (Local hostName port) s = (London, setEndpoint False hostName port s)

debugStdout :: IO Logger
debugStdout = newLogger Debug stdout

mkEnv :: Logging -> Credentials -> IO Env
-- Standard discovery mechanism for credentials, log to standard output
mkEnv LoggingEnabled c = do
  logger <- debugStdout
  newEnv c <&> set envLogger logger
-- Standard discovery mechanism for credentials, no logging
mkEnv LoggingDisabled c = newEnv c

withAWS ::
  (MonadBaseControl IO m, SessionClass b)
  => AWST' Env (ResourceT m) a
  -> b
  -> m a
withAWS action session =
  let Session {..} = rawSession session
  in runResourceT . runAWST _sEnv . within _sRegion $ do
    reconfigure _sService action
