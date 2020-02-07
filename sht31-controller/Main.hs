{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

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

import Support (subscribe, persist)

data Reading = Reading { defId :: Word32, temp :: Int32, humidity :: Word32, denominator :: Word32 }
    deriving (Generic, Show)

instance ToJSON Reading where

parse :: [Word8] -> Reading
parse [defId, tMsb, tLsb, _, hMsb, hLsb, _] =
    Reading { temp = fromIntegral $ shiftR (4375 * (shiftL tMsb32 8 .|. tLsb32)) 14 - 4500
            , humidity = shiftR (625 * (shiftL hMsb32 8 .|. hLsb32)) 12
            , denominator = 100
            , defId = fromIntegral defId
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

respond "controller.44" payload = encode $ parse payload

respond "global" msg =
    encode $ object
        [ "busAddress" .= (0x44 :: Word8)
        , "humanFriendlyName" .= ("SHT31" :: T.Text)
        , "readDefinitions" .=
            [ object
                [ "definitionId" .= (1 :: Word8)
                , "registerIdLength" .= (16  :: Word8)
                , "registerId" .= (0x2400 :: Word16)
                , "registerBlockLength" .= (1 :: Word8)
                , "numBytesPerRegister" .= (6 :: Word8)
                , "readPeriod" .= (500 :: Word32)
                ]
            ]
        ]

-- This will never happen but Haskell doesn't know that.
respond _ _ = undefined

main :: IO ()
main = do
    conn <- subscribe respond
    persist conn
