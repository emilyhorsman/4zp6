{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Aeson
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.AMQP as AMQP
import System.Environment (getEnv)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Support

e key = T.pack <$> getEnv key

test :: B.ByteString
test = BL.toStrict $ B64.encode $ BL.pack [1, 0x60, 0xff, 0x78, 0x58, 0x71, 0x98]

payload :: DataMessage
payload = DataMessage 1 68 (decodeUtf8 test)

go = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    publishMsg chan exchangeName "controller.44.#" AMQP.newMsg {msgBody = (encode payload)}
    AMQP.closeConnection conn

main :: IO ()
main = do
    go
    putStrLn "Sent"
