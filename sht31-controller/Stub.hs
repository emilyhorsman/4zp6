{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.AMQP as AMQP
import System.Environment (getEnv)

e key = T.pack <$> getEnv key

test = [1, 0x60, 0xff, 0x78, 0x58, 0x71, 0x98]

go = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    publishMsg chan exchangeName "controller.44" AMQP.newMsg {msgBody = (BL.pack test)}
    AMQP.closeConnection conn

main :: IO ()
main = do
    go
    putStrLn "Sent"
