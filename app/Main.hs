module Main where

import Lib
import Control.Lens

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Vector as V
import Data.Scientific (toRealFloat)
import Data.Maybe (fromMaybe)

tickToCoords :: Tick -> (Int, Double)
tickToCoords t = (fromMaybe 0 $ timestamp t, toRealFloat $ closeV t)

reconcile :: (Int, Double) -> Double -> (Int, Double)
reconcile (coord, _) v = (coord, v)

main :: IO ()
main = do
  --r <- makeAuthRequest "https://bittrex.com/api/v1.1/public/getmarkets"
  --putStrLn . show $ r ^. responseBody
  ticks <- getTicks
  let ticksData = fromMaybe (APIResponse True "" V.empty) ticks
      tickCoords = fmap tickToCoords (result ticksData)
      smoothed = V.zipWith reconcile tickCoords (gaussianSmooth 100 $ fmap snd tickCoords)

  toFile (def & fo_size .~ (1500, 1500)) "plot.png" $ do
    plot (line "smoothed" [V.toList smoothed])
    plot (line "original" [V.toList tickCoords])
