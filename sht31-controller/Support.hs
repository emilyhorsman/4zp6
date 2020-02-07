{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Support (subscribe, persist) where

import Control.Monad (void)
import Data.Word (Word8)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.AMQP as AMQP

e key = T.pack <$> getEnv key

type RespondFunc = T.Text -> [Word8] -> Maybe BL.ByteString

subscribe :: [T.Text] -> RespondFunc -> IO AMQP.Connection
subscribe busAddresses respondFunc = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    (queueName, _, _) <- AMQP.declareQueue chan
        AMQP.newQueue {AMQP.queueAutoDelete = True}

    AMQP.declareExchange chan
        AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "topic"}

    let controllerKeys = map (T.append "controller.") busAddresses
    mapM_ (AMQP.bindQueue chan queueName exchangeName) controllerKeys
    AMQP.bindQueue chan queueName exchangeName "global"

    AMQP.consumeMsgs chan queueName AMQP.Ack $ handle respondFunc chan

    return conn

handle :: RespondFunc -> AMQP.Channel -> (AMQP.Message, AMQP.Envelope) -> IO ()
handle respondFunc chan (AMQP.Message {AMQP.msgBody}, env@AMQP.Envelope {AMQP.envRoutingKey}) = do
    exchangeName <- e "EXCHANGE"
    let response = respondFunc envRoutingKey (BL.unpack msgBody)
    print response
    case response of
        Nothing ->
            return Nothing
        Just res ->
            AMQP.publishMsg chan exchangeName "backend"
                AMQP.newMsg {AMQP.msgBody = res}

    AMQP.ackEnv env

persist :: AMQP.Connection -> IO ()
persist conn = getLine >> AMQP.closeConnection conn
