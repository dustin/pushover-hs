{-|
Module      : Pushover
Description : An interface to NotifyMyAndroid.
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental

This is an interface to Pushover.net.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.API.Pushover (
  -- * Functions interacting with the the Pushover service.
  sendMessage, getReceipt,
  -- * Message ingredients.
  message,
  -- * Additional fields for emergency requests.
  emergency, eparams, retry, expire, callback, tags,
  Message(..), PriorityLevel(..), Sound(..),
  token, user, body, attachment, device, title, url, urlTitle, priority, sound,
  -- * Response to sendMessage
  Response(..),
  status, request, receipt, errors,
  -- Receipt stuff.
  Acknowledgment(..), Receipt(..),
  ackAt, ackBy, ackOn, acknowledged, calledBackAt, expired, expiresAt, lastDeliveredAt,
   -- For testing
   parseResponse
  ) where

import           Control.Exception         (throwIO)
import           Control.Lens
import           Control.Monad             (guard, void)
import           Control.Monad.Combinators (option)
import           Data.Aeson                (FromJSON (..), Value (..),
                                            eitherDecode, (.:))
import           Data.Aeson.Types          (typeMismatch)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import           Data.Default              (Default, def)
import           Data.Maybe                (fromJust)
import           Data.Scientific           (Scientific, toBoundedInteger)
import           Data.Text                 (Text, pack, toLower, unpack)
import           Data.Time                 (UTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import           Data.Time.Clock.System    (SystemTime (..), systemToUTCTime)
import           Generics.Deriving.Base    (Generic)
import           Network.HTTP.Client       (HttpException (..),
                                            HttpExceptionContent (..))
import           Network.Wreq              (FormParam (..), checkResponse,
                                            defaults, getWith, postWith,
                                            responseBody, responseStatus,
                                            statusCode)
import           Network.Wreq.Types        (Options, ResponseChecker)

-- | EmergencyParams provides parameters necessary for sending an
-- emergency message.  See 'emergency' for a default emergency
-- PriorityLevel.
data EmergencyParams = EmergencyParams {
  _retry      :: Int
  , _expire   :: Int
  , _callback :: Text
  , _tags     :: Text
  } deriving (Generic, Show, Eq)

makeLenses ''EmergencyParams

-- | Priority levels for a notification.
data PriorityLevel = VeryLow | Low | Normal | High | Emergency EmergencyParams
  deriving(Generic, Eq, Show)

-- | eparams gets or sets emergency parameters through a PriorityLevel
-- (which may or may not be an Emergency).
eparams :: Lens' PriorityLevel (Maybe EmergencyParams)
eparams = lens r w
  where
    r (Emergency p) = Just p
    r _             = Nothing

    w p Nothing              = p
    w (Emergency _) (Just v) = Emergency $ v
    w p _                    = p

-- | An Emergency PriorityLevel with default EmergencyParams.
emergency :: PriorityLevel
emergency = Emergency EmergencyParams{_retry=300,  _expire=3600, _callback="", _tags=""}

data Sound = None | Bike | Bugle | CashRegister | Classical
           | Cosmic | Falling | GameLAN | Incoming | Intermission
           | Magic | Mechanical | PianoBar | Siren | SpaceAlarm
           | Tugboat | Alien | Climb | Persistent | Echo | UpDown
           deriving (Show, Eq)

-- | A message to be sent via 'sendMessage'.
data Message = Message {
  _token        :: Text
  , _user       :: Text
  , _body       :: Text
  , _attachment :: B.ByteString
  , _device     :: Text
  , _title      :: Text
  , _url        :: Text
  , _urlTitle   :: Text
  , _priority   :: PriorityLevel
  , _sound      :: Sound
  , _timestamp  :: Maybe UTCTime
  } deriving  (Show, Generic)

makeLenses ''Message

instance Default Message where
  def = Message{
    _token = mempty
    , _user = mempty
    , _body = mempty
    , _attachment = mempty
    , _device = mempty
    , _title = mempty
    , _url = mempty
    , _urlTitle = mempty
    , _priority  = Normal
    , _sound = None
    , _timestamp = Nothing
    }

params :: Message -> [FormParam]
params m = [
  "token" := m ^. token,
  "user" := m ^. user,
  "message" := m ^. body,
  -- TODO: attachment
  "device" := m ^. device,
  "title" := m ^. title,
  "url" := m ^. url,
  "url_title" := m ^. urlTitle,
  "sound" := (toLower . pack . show $ (m ^. sound)),
  "priority" := (pval $ m ^. priority)
  ] <> utcToP (m ^. timestamp)
  <> pparams (m ^. priority)

  where
    utcToP Nothing = []
    utcToP (Just t) = ["timestamp" := (show . fromEnum . realToFrac . utcTimeToPOSIXSeconds $ t)]

    pparams p@(Emergency _) = ["retry" := p ^? eparams._Just.retry,
                               "expire" := p ^? eparams._Just.expire,
                               "callback" := p ^? eparams._Just.callback,
                               "tags" := p ^? eparams._Just.tags]
    pparams _ = []

    pval :: PriorityLevel -> Int
    pval VeryLow       = -2
    pval Low           = -1
    pval Normal        = 0
    pval High          = 1
    pval (Emergency _) = 2

-- | Build a message with defaults given an API Token, User ID, and initial message.
message :: Text -> Text -> Text ->  Message
message t u b = def{_token=t, _user=u, _body=b}

-- | The response to a 'sendMessage' request.
data Response = Response { _status  :: Int
                         , _request :: Text
                         , _receipt :: Text
                         , _errors  :: [Text]
                         } deriving (Show, Eq)

makeLenses ''Response

instance FromJSON Response where
  parseJSON (Object v) = Response
    <$> v .: "status"
    <*> v .: "request"
    <*> option "" (v .: "receipt")
    <*> option [] (v .: "errors")
  parseJSON invalid    = typeMismatch "Response" invalid

parseResponse :: L.ByteString -> Either Response Response
parseResponse b = case eitherDecode b of
                    Left x -> Left (Response{_status=0, _request="", _receipt="", _errors=[pack x]})
                    Right b' -> if b' ^.status == 1 then Right b' else Left b'

statusCheck :: [Int] -> ResponseChecker
statusCheck o _ r
  | r ^. responseStatus . statusCode `elem` o = pure ()
statusCheck _ req res = do
  bod <- res ^. responseBody
  throwIO $ HttpExceptionRequest req (StatusCodeException (void res) bod)

opts :: Options
opts = defaults & checkResponse .~ Just (statusCheck [200, 400])

transmit :: String -> Message -> IO (Either Response Response)
transmit u note = do
  r <- postWith opts u (params note)
  pure $ parseResponse $ r ^. responseBody

-- | Send a message.
sendMessage :: Message -> IO (Either Response Response)
sendMessage = transmit "https://api.pushover.net/1/messages.json"

stime :: Scientific -> UTCTime
stime = systemToUTCTime . flip MkSystemTime 0 . fromJust . toBoundedInteger

-- | An acknowledgment of an emergency message.
data Acknowledgment = Acknowledgment{
  _ackAt   :: UTCTime
  , _ackBy :: Text
  , _ackOn :: Text
  } deriving(Generic, Eq, Show)

makeLenses ''Acknowledgment

-- | Receipt represents the current status of receipt of an emergency message.
data Receipt = Receipt{
  _acknowledged      :: Maybe Acknowledgment
  , _lastDeliveredAt :: Maybe UTCTime
  , _expired         :: Bool
  , _expiresAt       :: Maybe UTCTime
  , _calledBackAt    :: Maybe UTCTime
    } deriving(Generic, Eq, Show)

makeLenses ''Receipt

instance FromJSON Receipt where
  parseJSON (Object v) = do
    sok <- pbool "status"
    guard sok

    sakd <- pbool "acknowledged"
    let ack = if sakd then parseAck else pure Nothing
    Receipt
      <$> ack
      <*> ptime "last_delivered_at"
      <*> pbool "expired"
      <*> ptime "expires_at"
      <*> ptime "called_back_at"

      where
        zero, one :: Scientific
        zero = 0
        one = 1

        pbool s = v .: s >>= \n -> pure (n == one)

        ptime s = do
          n <- v .: s
          if n == zero then pure Nothing else pure $ Just (stime n)

        ptime' s = stime <$> v .: s

        parseAck = do
          ack <- Acknowledgment
                 <$> ptime' "acknowledged_at"
                 <*> v .: "acknowledged_by"
                 <*> v .: "acknowledged_by_device"
          pure (Just ack)

  parseJSON invalid    = typeMismatch "Response" invalid

-- | Get the Receipt for an emergency message.
getReceipt :: Text -> Text -> IO (Either String Receipt)
getReceipt tok recpt = do
  let u = mconcat ["https://api.pushover.net/1/receipts/", recpt, ".json?token=", tok]
  r <- getWith opts (unpack u)
  pure $ eitherDecode $ r ^. responseBody


