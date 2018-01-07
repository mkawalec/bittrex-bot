module Main where

import Lib
import Control.Lens

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Scientific (toRealFloat)
import Data.Maybe (fromMaybe)
import System.TimeIt (timeIt)
import Control.DeepSeq (deepseq)

tickToCoords :: Tick -> (Int, Double)
tickToCoords t = (fromMaybe 0 $ timestamp t, closeV t)

reconcile :: (a, c) -> b -> (a, b)
reconcile (coord, _) v = (coord, v)

main :: IO ()
main = do
  ticks <- timeIt getTicks
  let ticksData = fromMaybe (APIResponse True "" V.empty) ticks
      tickCoords = fmap tickToCoords (result ticksData)
      unboxedticks = VU.fromList $ V.toList $ tickCoords

  smooth <- unboxedticks `deepseq` timeIt $ do
    let smoothed = VU.zipWith reconcile unboxedticks (gaussianSmooth 100 $ VU.map snd unboxedticks)
    return $! smoothed `deepseq` smoothed

  timeIt $ toFile (def & fo_size .~ (1500, 1500)) "plot.png" $ do
    plot (line "smoothed" [VU.toList smooth])
    plot (line "original" [V.toList tickCoords])
