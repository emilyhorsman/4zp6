{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Support (subscribe, persist) where

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

instance FromJSON DataMessage where
    parseJSON (Object v) =
        DataMessage <$>
        v .: "busId" <*>
        v .: "busAddr" <*>
        v .: "data"
    parseJSON _ = mzero

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


reply :: (T.Text -> BL.ByteString -> IO ()) -> T.Text -> Maybe T.Text -> IO ()
reply pub _ Nothing = return ()
reply pub routingKey (Just s) = pub (T.replace "controller" "data" routingKey) $ BL.fromStrict $ encodeUtf8 s

handleData :: ToJSON a => (T.Text -> BL.ByteString -> IO ()) -> ([Word8] -> Maybe a) -> (AMQP.Message, AMQP.Envelope) -> IO ()
handleData pub func (AMQP.Message {AMQP.msgBody}, env@AMQP.Envelope {AMQP.envRoutingKey}) = do
    case (decode msgBody :: Maybe DataMessage) of
        Nothing -> putStrLn "Failed to parse"
        Just result ->
            reply pub envRoutingKey (decodeUtf8 <$> BL.toStrict <$> encode <$> func (map c2w $ T.unpack $ message result))
    AMQP.ackEnv env
    return ()

interface :: (ToJSON a, ToJSON b) => T.Text -> ([Word8] -> Maybe a) -> b -> IO ()
interface busAddress handleFunc capabilities = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    AMQP.declareExchange chan
        AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "topic"}

    let pub key msg = void $ AMQP.publishMsg chan exchangeName key $ AMQP.newMsg { AMQP.msgBody = msg }
    -- Initial capabilities advertisement
    let advertise = void $ AMQP.publishMsg chan exchangeName "global.conf" $ AMQP.newMsg {AMQP.msgBody = encode capabilities}
    advertise
    -- Subscribe to global.req for capabilities advertisement
    (globalReqQueueName, _, _) <- AMQP.declareQueue chan $
        AMQP.newQueue {AMQP.queueAutoDelete = True}
    AMQP.bindQueue chan globalReqQueueName exchangeName "global.req"
    AMQP.consumeMsgs chan globalReqQueueName AMQP.Ack $ \(_, env) -> advertise >> AMQP.ackEnv env

    (dataQueueName, _, _) <- AMQP.declareQueue chan $
        AMQP.newQueue {AMQP.queueAutoDelete = True}
    AMQP.bindQueue chan dataQueueName exchangeName $ T.intercalate "." ["controller", busAddress, "#"]
    AMQP.consumeMsgs chan dataQueueName AMQP.Ack (handleData pub handleFunc)

    persist conn
