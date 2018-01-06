module Lib
    ( someFunc
    , makeAuthRequest
    , getTicks
    ) where

import Control.Lens
import Control.Applicative

import Data.Aeson
import Network.Wreq
import Data.HMAC (hmac, HashMethod(..))
import Data.Digest.SHA512 (hash)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Builder (byteStringHex, toLazyByteString)

import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import GHC.Generics

apiSecret :: ByteString
apiSecret = "003dda1db8804f98bf6f4345b1e94dc7"

apiKey :: ByteString
apiKey = "b2767c02a7ca49b4a59004dea535c704"


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tick = Tick {
  baseVolume :: Scientific
, close :: Scientific
, high :: Scientific
, low :: Scientific
, open :: Scientific
, timestamp :: Maybe Int
, volume :: Scientific
} deriving (Show, Eq)

instance Ord Tick where
  compare a b = if a < b then LT else GT

data APIResponse a = APIResponse {
  success :: Bool
, message :: Text
, result :: a
} deriving (Show, Eq, Ord, Generic)

timeToTimestamp :: String -> Maybe Int
timeToTimestamp timeStr = round . utcTimeToPOSIXSeconds <$>
                          parseTimeM True defaultTimeLocale "%FT%T" timeStr

instance FromJSON Tick where
  parseJSON = withObject "Tick" $ \v -> Tick
    <$> v .: "BV"
    <*> v .: "C"
    <*> v .: "H"
    <*> v .: "L"
    <*> v .: "O"
    <*> fmap timeToTimestamp (v .: "T")
    <*> v .: "V"

instance FromJSON a => FromJSON (APIResponse a)

getTicks :: IO (Maybe (APIResponse (Vector Tick)))
getTicks = do
  time <- round <$> getPOSIXTime
  let url = BC.concat [
              "https://bittrex.com/Api/v2.0/pub/market/GetTicks"
            , "?marketName=BTC-ADA"
            , "&tickInterval=oneMin"
            , "&_="
            , BC.pack $ show time]

  r <- get (BC.unpack url)
  return . decode $ r ^. responseBody

makeAuthRequest :: ByteString -> IO (Response BL.ByteString)
makeAuthRequest url = do
  time <- round <$> getPOSIXTime
  let fullPath = BC.concat [
                url
              , "?apiKey="
              , apiKey
              , "&nonce="
              , BC.pack $ show time]
      signature = hmac (HashMethod hash 1024) (B.unpack apiSecret) (B.unpack fullPath)
      hexSignature = B.concat . BL.toChunks . toLazyByteString . byteStringHex . B.pack $ signature
      opts = defaults & header "apisign" .~ [hexSignature]
  getWith opts (BC.unpack fullPath)
