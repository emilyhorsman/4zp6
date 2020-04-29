{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Support (subscribe, persist) where

import Data.Aeson
import Control.Monad (mzero, void)
import Data.Word (Word8, Word32)
import GHC.Generics
import System.Environment (getEnv)
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

handleData :: (AMQP.Message, AMQP.Envelope) -> IO ()
handleData (AMQP.Message {AMQP.msgBody}, env@AMQP.Envelope {AMQP.envRoutingKey}) = do
    -- TODO: parse json, decode base64
    AMQP.ackEnv env
    return ()

interface :: ToJSON a => T.Text -> a -> IO ()
interface busAddress capabilities = do
    h <- getEnv "HOST"
    u <- e "USER"
    p <- e "PASS"
    exchangeName <- e "EXCHANGE"
    conn <- AMQP.openConnection h "/" u p
    chan <- AMQP.openChannel conn

    AMQP.declareExchange chan
        AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "topic"}

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
    AMQP.consumeMsgs chan dataQueueName AMQP.Ack handleData

    persist conn
