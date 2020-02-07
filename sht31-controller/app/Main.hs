{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Network.AMQP as AMQP
import System.Environment (getEnv)
import Data.Text (pack)

e key = pack <$> getEnv key

subscribe f = do
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
    AMQP.consumeMsgs chan queueName AMQP.Ack f

    return (conn, chan)

handle (msg, env@(AMQP.Envelope {envRoutingKey})) = do
    print msg
    print envRoutingKey
    AMQP.ackEnv env

main :: IO ()
main = do
    putStrLn "Hello"
    (conn, _) <- subscribe handle

    getLine
    AMQP.closeConnection conn
