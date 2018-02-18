{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module AWSViaHaskell.Classes
    ( ServiceClass(..)
    , SessionClass(..)
    , HasEnv (..)
    ) where

import AWSViaHaskell.Types
import Control.Lens (Lens')
import Network.AWS (Env, Service)

class ServiceClass a where
  type TypedSession a :: *
  rawService     ::       a -> Service
  wrappedSession :: Session -> TypedSession a

class SessionClass a where
  rawSession :: a -> Session

class HasEnv env where
  envL :: Lens' env Env
