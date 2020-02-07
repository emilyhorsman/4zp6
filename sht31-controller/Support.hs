{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Support (subscribe, persist) where

import Control.Monad (void)
import Network.AMQP as AMQP
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

e key = T.pack <$> getEnv key

subscribe respondFunc = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    (queueName, _, _) <- AMQP.declareQueue chan AMQP.newQueue {queueAutoDelete = True}
    AMQP.declareExchange chan AMQP.newExchange {exchangeName, exchangeType = "topic"}
    AMQP.bindQueue chan queueName exchangeName "controller.44"
    AMQP.bindQueue chan queueName exchangeName "global"
    AMQP.consumeMsgs chan queueName AMQP.Ack $ handle respondFunc chan

    return conn

handle respondFunc chan (msg@AMQP.Message {msgBody}, env@AMQP.Envelope {AMQP.envRoutingKey}) = do
    exchangeName <- e "EXCHANGE"
    let response = respondFunc envRoutingKey (BL.unpack msgBody)
    print response
    case response of
        Nothing ->
            return Nothing
        Just res ->
            AMQP.publishMsg chan exchangeName "backend" AMQP.newMsg {msgBody = res}

    AMQP.ackEnv env

persist conn = do
    getLine
    AMQP.closeConnection conn
