{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson 
import Data.Array
import Data.Monoid 
import Data.Text 

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as LBase64

import GHC.Generics

import Network.AMQP as AMQP
import Network

import Network.Memcache (Memcache)
import qualified Network.Memcache
import Network.Memcache.Protocol as Single
import Network.Memcache.Serializable(Serializable(..))

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)
    
import Blaze.ByteString.Builder (copyByteString)
import System.Random

import Data.Time.Format
import Data.Time.Clock
import Data.Time
import System.Locale

data Config = Config {
    appPort :: !Int,
    memcacheHost :: !String,
    memcachePort :: !Int,
    memcachePrefix :: !String,
    amqpHost :: !String,
    amqpVHost :: !Text,
    amqpUser :: !Text,
    amqpPassword :: !Text,
    amqpQueueName :: !Text,
    amqpExchangeName :: !Text,
    amqpKey :: !Text
} deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

data Hit = Hit { 
    banner :: String,
    slot :: String,
    campaign :: String,
    code :: String
} deriving (Show, Generic)

instance FromJSON Hit
instance ToJSON Hit

data Global = Global {
    conn :: Connection,
    serv :: Server
}
configFile :: FilePath
configFile = "config.json"

main :: IO ()
main = do
    c <- (eitherDecode <$> readJSON configFile) :: IO (Either String Config)
    case c of 
        Left err -> putStrLn err
        Right c -> do 
            memcache <- initMemcache c
            conn <- initAmqp c
            
            runApp c c memcache conn
            
            closeConnections conn memcache

readJSON :: FilePath -> IO (BL.ByteString)
readJSON fileName = BL.readFile fileName

    
initMemcache :: Config -> IO (Server)
initMemcache (Config { 
        memcacheHost = memcacheHost,
        memcachePort = memcachePort
        }) = do
    -- @todo FIXME Int -> PortNumber
    server <- Single.connect memcacheHost 11211
    return server

initAmqp :: Config -> IO (Connection)
initAmqp (Config { 
        amqpHost = amqpHost,
        amqpVHost = amqpVHost,
        amqpUser = amqpUser,
        amqpPassword = amqpPassword,
        amqpQueueName = amqpQueueName,
        amqpExchangeName = amqpExchangeName,
        amqpKey = amqpKey}) = do
    conn <- AMQP.openConnection amqpHost amqpVHost amqpUser amqpPassword
    chan <- AMQP.openChannel conn
    --queues
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName = amqpQueueName}
    AMQP.declareExchange chan 
                         AMQP.newExchange {AMQP.exchangeName = amqpExchangeName, 
                                           AMQP.exchangeType = "fanout"}
    AMQP.bindQueue chan amqpQueueName amqpExchangeName amqpKey
    return conn

closeConnections :: Connection -> Server -> IO ()
closeConnections conn memcache = do
    AMQP.closeConnection conn
    Single.disconnect memcache 


runApp :: Config -> Config -> Server -> Connection -> IO ()
runApp (Config {appPort = appPort}) config srv conn = do
    putStrLn $ "Running on port " ++ (show appPort) ++ "..."
    run appPort $ application config srv conn

application :: Config -> Server -> Connection -> Application
application conf srv conn req = do
    response <- lift $ handle conf srv conn $ Prelude.head $ pathInfo req

    return $
        case pathInfo req of
            [] -> yay
            ["yay"] -> yay
            x -> process response
yay :: Response
yay = ResponseBuilder status200 [("Content-type", "text/plain")] $
    mconcat $ Prelude.map copyByteString ["yay"]

process:: BL.ByteString -> Response
process x = responseLBS status200 [("Content-type", "text/plain")] x

handle :: Config -> Server -> Connection -> Text -> IO BL.ByteString
handle conf srv conn slotId = do 
    --meat!
    json <- getJsonString conf srv slotId
    let x = decode (BL.pack json) :: Maybe [Hit]
    
    case x of
        Nothing -> do
            return $ BL.pack ""
        Just x -> do
            z <- pick x
            msg <- encodeHit z
            --amqp
            publishMessage conf conn msg
            return (BL.pack (getHitCode z))

publishMessage :: Config -> Connection -> String -> IO ()
publishMessage (Config{
        amqpExchangeName = amqpExchangeName,
        amqpKey = amqpKey
    }) conn msg = do
    
    chan <- AMQP.openChannel conn
    AMQP.publishMsg chan amqpExchangeName amqpKey
        AMQP.newMsg { AMQP.msgBody = (LBase64.encode (BL.pack (msg)) ),
                      AMQP.msgDeliveryMode = Just AMQP.Persistent}

getJsonString :: (Memcache mc) => Config -> mc -> Text -> IO String
getJsonString (Config {memcachePrefix = memcachePrefix}) memcache key = do
    let mckey = memcachePrefix ++ ( Data.Text.unpack key )
    --print mckey
    json <- Network.Memcache.get memcache mckey
    case json of
            Nothing -> return mzero
            Just v -> return v

-- @todo make dependent on current ML algo (module?)
pick :: [a] -> IO a
pick xs = randomRIO (0, (Prelude.length xs - 1)) >>= return . (xs !!)   

-- @todo rewrite!
encodeHit :: Hit -> IO String
encodeHit (Hit {banner = b, slot = s, campaign = c, code = code}) = do
    now <- getZonedTime
    let stamp = formatTime defaultTimeLocale "%Y%m%d%H00" now
    return ("{\"banner\":\"" ++ b ++ "\",\"slot\":\"" ++ s ++ "\",\"campaign\":\"" ++ c ++ "\", \"stamp\":\"" ++ stamp ++ "\"}")

getHitCode :: Hit -> String
getHitCode (Hit {banner = b, slot = s, campaign = c, code = code}) = code
