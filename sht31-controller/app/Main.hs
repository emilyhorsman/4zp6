{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.AMQP as AMQP
import System.Environment (getEnv)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Data.Bits

data Reading = Reading { temp :: Int32, humidity :: Word32, denominator :: Word32 }
    deriving Show

e key = T.pack <$> getEnv key

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

handle (msg@(AMQP.Message {msgBody}), env@(AMQP.Envelope {AMQP.envRoutingKey})) = do
    print msg
    print envRoutingKey
    respond envRoutingKey (BL.unpack msgBody)
    AMQP.ackEnv env

parse :: [Word8] -> Reading
parse [tMsb, tLsb, _, hMsb, hLsb, _] =
    Reading { temp = fromIntegral $ (shiftR (4375 * (shiftL tMsb32 8 .|. tLsb32)) 14) - 4500
            , humidity = shiftR (625 * (shiftL hMsb32 8 .|. hLsb32)) 12
            , denominator = 100
            }
    where
        tMsb32 :: Word32
        tMsb32 = fromIntegral tMsb

        tLsb32 :: Word32
        tLsb32 = fromIntegral tLsb

        hMsb32 :: Word32
        hMsb32 = fromIntegral hMsb

        hLsb32 :: Word32
        hLsb32 = fromIntegral hLsb
parse _ = undefined

respond "controller.44" (defId : message) = do
    print $ parse message
    print defId
    print message

respond "global" msg = print "aaaaa"

-- This will never happen but Haskell doesn't know that.
respond _ _ = undefined

main :: IO ()
main = do
    putStrLn "Hello"
    (conn, _) <- subscribe handle

    getLine
    AMQP.closeConnection conn
