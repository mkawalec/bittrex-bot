module Lib
    ( someFunc
    , makeAuthRequest
    , getTicks
    , Tick(..)
    , APIResponse(..)
    , gaussianSmooth
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
import qualified Data.Vector as V
import GHC.Generics

apiSecret :: ByteString
apiSecret = "003dda1db8804f98bf6f4345b1e94dc7"

apiKey :: ByteString
apiKey = "b2767c02a7ca49b4a59004dea535c704"


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tick = Tick {
  baseVolume :: Scientific
, closeV :: Scientific
, highV :: Scientific
, lowV :: Scientific
, openV :: Scientific
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

convolve :: Vector Double -> Vector Double -> Int -> Vector Double
convolve a b idx = initial V.// updates
    where initial = V.replicate (V.length a) 0.0
          updates = map convolve' [(max 0 idx)..(min (V.length a - 1) (idx + V.length b - 1))]

          convolve' :: Int -> (Int, Double)
          convolve' i = (i, a V.! i * b V.! (i - idx))

gaussianSmooth :: Int -> Vector Double -> Vector Double
gaussianSmooth omega input = V.fromList $ map V.sum convolutions
  where
        convolutions = map (convolve padded mask) [0..(inputL - 1)]
        mask = V.generate (6 * omega) $
                  \i -> gauss (fromIntegral omega) (fromIntegral . abs $ 3 * omega - i)

        inputL = V.length input
        padded = V.generate (6 * omega + inputL) $
                  \i -> if i < 3 * omega
                          then input V.! 0
                          else if i >= inputL + 3 * omega
                                then input V.! (inputL - 1)
                                else input V.! (i - 3 * omega)


gauss :: Double -> Double -> Double
gauss omega x = let normalizer = 1 / (sqrt $ 2 * pi * (omega ** 2))
                in normalizer * (exp $ - (x ** 2) / (2 * (omega ** 2)))


getTicks :: IO (Maybe (APIResponse (Vector Tick)))
getTicks = do
  time <- round <$> getPOSIXTime
  let url = BC.concat [
              "https://bittrex.com/Api/v2.0/pub/market/GetTicks"
            , "?marketName=BTC-ETH"
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
