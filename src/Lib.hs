module Lib
    ( someFunc
    , makeAuthRequest
    , getTicks
    , Tick(..)
    , APIResponse(..)
    , gaussianSmooth
    , findExtrema
    , Extremum(..)
    , ExtremumType(..)
    , Budget(..)
    , History(..)
    , simulate
    , holdAfterPeakBot
    , mapFst
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
import qualified Data.Vector.Unboxed as VU
import Data.List (foldl')

import GHC.Generics
import Control.DeepSeq
import qualified Debug.Trace as DT

apiSecret :: ByteString
apiSecret = "003dda1db8804f98bf6f4345b1e94dc7"

apiKey :: ByteString
apiKey = "b2767c02a7ca49b4a59004dea535c704"


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tick = Tick {
  baseVolume :: Double
, closeV :: Double
, highV :: Double
, lowV :: Double
, openV :: Double
, timestamp :: Maybe Int
, volume :: Double
} deriving (Show, Eq, Generic, NFData)

instance Ord Tick where
  compare a b = if timestamp a < timestamp b then LT else GT

data APIResponse a = APIResponse {
  success :: Bool
, message :: Text
, result :: a
} deriving (Show, Eq, Ord, Generic, Generic1, NFData, NFData1)

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

instance ToJSON Tick where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (APIResponse a)
instance ToJSON a => ToJSON (APIResponse a) where
  toEncoding = genericToEncoding defaultOptions

convolveSum :: VU.Vector Double -> VU.Vector Double -> Int -> Double
{-# INLINE convolveSum #-}
convolveSum a b idx = foldl' convolveSum' 0 [start..end]
    where start = max 0 idx
          end = min (VU.length a - 1) (idx + VU.length b - 1)

          convolveSum' :: Double -> Int -> Double
          {-# INLINE convolveSum' #-}
          convolveSum' acc i = acc + a VU.! i * b VU.! (i - idx)

convolve :: VU.Vector Double -> VU.Vector Double -> Int -> VU.Vector Double
{-# INLINE convolve #-}
convolve a b idx = VU.generate (VU.length a) convolve'
  where start = max 0 idx
        end = min (VU.length a - 1) (idx + VU.length b - 1)

        convolve' :: Int -> Double
        {-# INLINE convolve' #-}
        convolve' i = if i >= start && i <= end
                      then a VU.! i * b VU.! (i - idx)
                      else 0


gaussianSmooth :: Int -> VU.Vector Double -> VU.Vector Double
{-# INLINE gaussianSmooth #-}
gaussianSmooth omega input = VU.generate (VU.length input) (convolveSum padded kernel)
  where (padded, kernel) = gaussianPrepare omega input

gaussianPrepare :: Int -> VU.Vector Double -> (VU.Vector Double, VU.Vector Double)
{-# INLINE gaussianPrepare #-}
gaussianPrepare omega input = (padded, kernel)
  where
        kernel = VU.generate (6 * omega) $!
                  \i -> gauss (fromIntegral omega) (fromIntegral . abs $ 3 * omega - i)

        inputL = VU.length input
        padded = VU.generate (6 * omega + inputL) $!
                  \i -> if i < 3 * omega
                          then input VU.! 0
                          else if i >= inputL + 3 * omega
                                then input VU.! (inputL - 1)
                                else input VU.! (i - 3 * omega)

data Action = Sell | Buy | Hold deriving (Show, Eq, Ord)
data History = History Action Int
  deriving (Show, Eq, Ord)

data Budget = Budget {
  baseCurr :: Rational
, boughtCurr :: Rational
} deriving (Show, Eq, Ord)

holdAfterPeakBot :: Double -> VU.Vector Double -> Budget -> Action
holdAfterPeakBot epsilon exchangeRate (Budget base bought) = case last extrema of
    Extremum Minimum i -> let rateAtExtremum = exchangeRate VU.! i
                          in if base > 0 && (currentRate / rateAtExtremum - 1) > epsilon
                             then Buy
                             else Hold
    Extremum Maximum i -> let rateAtExtremum = exchangeRate VU.! i
                          in if bought > 0 && abs (1 - currentRate / rateAtExtremum) > epsilon
                             then Sell
                             else Hold
    _ -> Hold
  where extrema = findExtrema exchangeRate
        currentRate = VU.last exchangeRate

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

simulate :: forall a. Int -> Budget -> VU.Vector Double ->
                      (VU.Vector Double -> Budget -> Action) ->
                      (Rational, [History])
simulate omega budget history bot =
    mapFst (currentValueInBase history) $ foldl' (uncurry step) (budget, []) range
  where
   range = [6 * omega..(VU.length history - 1)]
   {-# INLINE smoothRange #-}
   smoothRange i = gaussianSmooth omega (VU.slice 0 i history)
   exchangeFee = 1 - 0.0025

   currentValueInBase hist b@(Budget base bought) = DT.traceShow b $ base + bought *
      (toRational $ VU.last hist) * exchangeFee

   step :: Budget -> [History] -> Int -> (Budget, [History])
   {-# INLINE step #-}
   step b@(Budget base bought) h endIdx =
      case action of
        Sell -> (Budget (base + bought * currentPrice * exchangeFee) 0, (History Sell endIdx):h)
        Buy -> (Budget 0 (bought + base / currentPrice * exchangeFee), (History Buy endIdx):h)
        Hold -> (b, h)
     where smoothed = smoothRange endIdx
           action = bot smoothed b
           currentPrice = toRational $ history VU.! endIdx


gauss :: Double -> Double -> Double
gauss omega x = let normalizer = 1 / (sqrt $ 2 * pi * (omega ** 2))
                in normalizer * (exp $ - (x ** 2) / (2 * (omega ** 2)))

data ExtremumType = Minimum | Switch | Maximum deriving (Show, Eq, Ord)
data Extremum = Extremum {
  extremumType :: ExtremumType
, index :: Int
} deriving (Show, Eq, Ord)

findExtrema :: VU.Vector Double -> [Extremum]
findExtrema input =
  let offset = VU.head input `VU.cons` input
      der = VU.zipWith (-) input offset

      findExtrema' :: [Extremum] -> Int -> [Extremum]
      {-# INLINE findExtrema' #-}
      findExtrema' xs i
        | i > 0 && i < VU.length der - 1 =
            if der VU.! (i - 1) < 0 && der VU.! i > 0
              then (Extremum Minimum i):xs
              else if der VU.! (i - 1) > 0 && der VU.! i < 0
                   then (Extremum Maximum i):xs
                   else xs
        | otherwise = xs
  in reverse $ foldl' findExtrema' [] [0..(VU.length der - 1)]


getTicks :: ByteString -> IO (Maybe (APIResponse (Vector Tick)))
getTicks market = do
  time <- round <$> getPOSIXTime
  let url = BC.concat [
              "https://bittrex.com/Api/v2.0/pub/market/GetTicks"
            , "?marketName="
            , market
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
