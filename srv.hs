{-# LANGUAGE OverloadedStrings #-}


import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as LBase64

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Array
import Data.Monoid 
import qualified Data.Text as T

import Data.Time.Format
import Data.Time.Clock
import Data.Time
import System.Locale

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


import Network.AMQP as AMQP

import Network.Memcache (Memcache)
import qualified Network.Memcache
import Network.Memcache.Protocol as Single
import Network.Memcache.Serializable(Serializable(..))

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)

import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU

import System.Random

import Data.Maybe

data Hit = Hit { 
    banner :: String,
    slot :: String,
    campaign :: String,
    code :: String
} deriving (Show)

process v = Hit <$>
    (v .: "banner") <*>
    (v .: "slot") <*>
    (v .: "campaign") <*>
    (v .: "code")

instance FromJSON Hit where
    parseJSON (Object o) = process o
    parseJSON (Array a) = mzero
    parseJSON _ = mzero

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Running on port " ++ show port ++ "..."
    run port application

application req = do
    response <- lift $ handle $ head $ pathInfo req
    return $
        case pathInfo req of
            [] -> yay
            ["yay"] -> yay
            x -> action response

yay :: Response
yay = ResponseBuilder status200 [("Content-type", "text/plain")] $
    mconcat $ map copyByteString ["yay"]

action :: BL.ByteString -> Response
action x = responseLBS status200 [("Content-type", "text/plain")] x

handle :: T.Text -> IO BL.ByteString
handle slotId = do
    ---INIT
    --memcached
    server <- Single.connect "localhost" 11211

    json <- getJsonString server slotId

    Single.disconnect server
    --let x = decode (BL.pack json) :: Maybe [Hit]
    let x = decode (BL.pack json) :: Maybe [Hit]
        
    case x of
        Nothing -> do
            return $ BL.pack ""
        Just x -> do
            z <- pick x
            msg <- encodeHit z
            --amqp
            conn <- AMQP.openConnection "127.0.0.1" "galahad" "guest" "guest"
            chan <- AMQP.openChannel conn
            --queues
            AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName = "amqp.hit.message.queue"}
            AMQP.declareExchange chan 
                                 AMQP.newExchange {AMQP.exchangeName = "amqp.hit.message.exchange", 
                                                   AMQP.exchangeType = "fanout"}
            AMQP.bindQueue chan "amqp.hit.message.queue" "amqp.hit.message.exchange" "myKey"
            AMQP.publishMsg chan "amqp.hit.message.exchange" "myKey"
                AMQP.newMsg { AMQP.msgBody = (LBase64.encode (BL.pack (msg)) ),
                              AMQP.msgDeliveryMode = Just AMQP.Persistent}
            AMQP.closeConnection conn
            
            return (BL.pack (getHitCode z))

getJsonString :: (Memcache mc) => mc ->T.Text -> IO String
getJsonString memcache key = do
    let mckey = "galahad_cache_" ++ (T.unpack key)
    --print mckey
    json <- Network.Memcache.get memcache mckey
    case json of
            Nothing -> return mzero
            Just v -> return v

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)   

encodeHit :: Hit -> IO String
encodeHit (Hit {banner = b, slot = s, campaign = c, code = code}) = do
    now <- getZonedTime
    let stamp = formatTime defaultTimeLocale "%Y%m%d%H00" now
    return ("{\"banner\":\"" ++ b ++ "\",\"slot\":\"" ++ s ++ "\",\"campaign\":\"" ++ c ++ "\", \"stamp\":\"" ++ stamp ++ "\"}")

getHitCode :: Hit -> String
getHitCode (Hit {banner = b, slot = s, campaign = c, code = code}) = code
