{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Support (DataMessage(..), interface) where

import Data.Aeson
import Control.Monad (mzero, void)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8, Word32)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import System.Environment (getEnv)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.AMQP as AMQP

data DataMessage = DataMessage
    { busId :: Word32
    , busAddr :: Word32
    , message :: T.Text
    }
    deriving (Generic, Show)

instance ToJSON DataMessage where
    toJSON (DataMessage busId busAddr message) = object
        [ "busId" .= busId
        , "busAddr" .= busAddr
        , "data" .= message
        ]

instance FromJSON DataMessage where
    parseJSON (Object v) =
        DataMessage <$>
        v .: "busId" <*>
        v .: "busAddr" <*>
        v .: "data"
    parseJSON _ = mzero

e key = T.pack <$> getEnv key

persist :: AMQP.Connection -> IO ()
persist conn = getLine >> AMQP.closeConnection conn

type PublishFunc = T.Text -> BL.ByteString -> IO ()

reply :: PublishFunc -> T.Text -> Maybe T.Text -> IO ()
reply pub _ Nothing = putStrLn "Failed to construct reply"
reply pub routingKey (Just s) = do
    putStrLn "Sending controller.data"
    pub (T.replace "controller" "data" routingKey) $ BL.fromStrict $ encodeUtf8 s

de :: T.Text -> [Word8]
de msg = BL.unpack $ B64.decodeLenient $ BL.fromStrict $ encodeUtf8 msg

handleData :: ToJSON a => PublishFunc -> ([Word8] -> Maybe a) -> (AMQP.Message, AMQP.Envelope) -> IO ()
handleData pub func (AMQP.Message {AMQP.msgBody}, env@AMQP.Envelope {AMQP.envRoutingKey}) = do
    AMQP.ackEnv env
    putStrLn "Received data message"
    case (decode msgBody :: Maybe DataMessage) of
        Nothing -> putStrLn "Failed to parse"
        Just result -> do
            reply pub envRoutingKey (decodeUtf8 <$> BL.toStrict <$> encode <$> func (de (message result)))

interface :: (ToJSON a, ToJSON b) => T.Text -> ([Word8] -> Maybe a) -> b -> IO ()
interface busAddress handleFunc capabilities = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    putStrLn "Opening connection"
    conn <- AMQP.openConnection h "/" u p
    putStrLn "Opening channel"
    chan <- AMQP.openChannel conn
    putStrLn "Channel open"

    AMQP.declareExchange chan AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "topic"}

    let pub key msg = void $ AMQP.publishMsg chan exchangeName key $ AMQP.newMsg {AMQP.msgBody = msg}
    let advertise = void $ AMQP.publishMsg chan exchangeName "global.config" $ AMQP.newMsg {AMQP.msgBody = encode capabilities}
    advertise
    putStrLn "Sent capabilities advertisement"

    (globalReqQueueName, _, _) <- AMQP.declareQueue chan $
        AMQP.newQueue {AMQP.queueAutoDelete = True}
    AMQP.bindQueue chan globalReqQueueName exchangeName "global.req"
    AMQP.consumeMsgs chan globalReqQueueName AMQP.Ack $ \(_, env) -> putStrLn "global req msg" >> advertise >> AMQP.ackEnv env

    (dataQueueName, _, _) <- AMQP.declareQueue chan $
        AMQP.newQueue {AMQP.queueAutoDelete = True}
    AMQP.bindQueue chan dataQueueName exchangeName $ T.intercalate "." ["controller", busAddress, "#"]
    AMQP.consumeMsgs chan dataQueueName AMQP.Ack (handleData pub handleFunc)

    persist conn
