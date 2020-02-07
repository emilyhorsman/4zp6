{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Support (subscribe, persist) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.AMQP as AMQP
import System.Environment (getEnv)
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Bits
import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map

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
    AMQP.publishMsg chan exchangeName "backend" AMQP.newMsg {msgBody = response}
    AMQP.ackEnv env

persist conn = do
    getLine
    AMQP.closeConnection conn
