--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AWSViaHaskell
                    ( AWSAction
                    , AWSConfig(..)
                    , LoggingState(..)
                    , ServiceEndpoint(..)
                    , awsConfig
                    , getAWSConnection
                    , withAWS
                    )
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.))
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Network.AWS (send)
import           Network.AWS.SQS
                    ( _QueueDoesNotExist
                    , createQueue
                    , getQueueURL
                    , gqursQueueURL
                    , listQueues
                    , lqrsQueueURLs
                    , mBody
                    , receiveMessage
                    , rmrsMessages
                    , sendMessage
                    , sqs
                    )

newtype QueueName = QueueName Text deriving Show

newtype QueueURL = QueueURL Text deriving Show

doListQueues :: AWSAction [Text]
doListQueues = withAWS $ do
    result <- send $ listQueues
    return $ result ^. lqrsQueueURLs

doCreateQueue :: QueueName -> AWSAction ()
doCreateQueue (QueueName queueName) = withAWS (void $ send $ createQueue queueName)

doGetQueueURL :: QueueName -> AWSAction (Maybe QueueURL)
doGetQueueURL (QueueName queueName) = withAWS $ do
    handling _QueueDoesNotExist (const (pure Nothing)) $ do
        result <- send $ getQueueURL queueName
        return $ Just (QueueURL $ result ^. gqursQueueURL)

doSendMessage :: QueueURL -> Text -> AWSAction ()
doSendMessage (QueueURL s) m = withAWS $ do
    void $ send $ sendMessage s m

doReceiveMessage :: QueueURL -> AWSAction (Maybe Text)
doReceiveMessage (QueueURL s) = withAWS $ do
    result <- send $ receiveMessage s
    case result ^. rmrsMessages of
        m : [] -> return $ m ^. mBody
        _ -> return Nothing

main :: IO ()
main = do
    let queueName = QueueName "my-queue"

    awsInfo <- getAWSConnection $ (awsConfig (Local "localhost" 4576) sqs)
                                    { acLoggingState = LoggingDisabled }

    putStrLn "CreateQueue"
    doCreateQueue queueName awsInfo

    putStrLn "ListQueues"
    queueURLs <- doListQueues awsInfo
    forM_ queueURLs $ \queueURL ->
        Text.putStrLn $ "  " <> queueURL

    putStrLn "GetQueueURL"
    mbQueueURL <- doGetQueueURL queueName awsInfo
    case mbQueueURL of
        Nothing -> Text.putStrLn "  (not found)"
        Just queueURL -> do
            putStrLn $ "  " <> show queueURL

            putStrLn "SendMessage"
            doSendMessage queueURL "a message" awsInfo

            putStrLn "ReceiveMessage"
            m <- doReceiveMessage queueURL awsInfo
            putStrLn $ "  " <> show m