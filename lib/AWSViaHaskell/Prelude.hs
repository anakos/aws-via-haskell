module AWSViaHaskell.Prelude
    ( (^.)
    , (&)
    , (.~)
    , (<&>)
    , Lens'
    , _ServiceError
    , AsError
    , Credentials(..)
    , Region(..)
    , ServiceError
    , await
    , hasCode
    , hasStatus
    , send
    , set
    , sinkBody
    , toText
    ) where

import           Control.Lens
                   ( (^.)
                   , (&)
                   , (.~)
                   , (<&>)
                   , Lens'
                   , set
                   )
import           Network.AWS
                    ( _ServiceError
                    , AsError
                    , Credentials(..)
                    , Region(..)
                    , ServiceError
                    , await
                    , send
                    , sinkBody
                    )
import           Network.AWS.Data (toText)
import           Network.AWS.Error
                    ( hasCode
                    , hasStatus
                    )
