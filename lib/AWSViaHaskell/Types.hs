module AWSViaHaskell.Types
    ( Session(..)
    , sEnv
    , sRegion
    , sService
    ) where

import Protolude
import Control.Lens (Lens')
import Network.AWS (Env, Region, Service)

data Session = Session
    { _sEnv     :: Env
    , _sRegion  :: Region
    , _sService :: Service
    }

sEnv :: Lens' Session Env
sEnv f s@Session {..}     =
  (\e -> s { _sEnv = e }) <$> f _sEnv

sRegion :: Lens' Session Region
sRegion f s@Session {..}  =
  (\r -> s { _sRegion = r }) <$> f _sRegion

sService :: Lens' Session Service
sService f s@Session {..} =
  (\s' -> s { _sService = s' }) <$> f _sService
