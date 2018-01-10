module Main where

import Lib
import Control.Lens
import Data.Aeson (encode, decode)

import Control.Exception (catch, IOException)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Maybe (fromMaybe)
import System.TimeIt (timeIt)
import Control.DeepSeq (deepseq)

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import Data.List (partition)

tickToCoords :: Tick -> (Int, Double)
tickToCoords t = (fromMaybe 0 $ timestamp t, closeV t)

reconcile :: (a, c) -> b -> (a, b)
reconcile (coord, _) v = (coord, v)

serializeResults :: String -> [Tick] -> IO ()
serializeResults filename payload = B.writeFile filename (encode payload)

loadData :: String -> IO (Maybe (V.Vector Tick))
loadData path =
  (decode <$> B.readFile path)
    `catch`
  (\e -> return (e :: IOException) >> return Nothing)

fileName :: String
fileName = "btc-ada.json"

main :: IO ()
main = do
--  history <- S.fromList . V.toList . fromMaybe V.empty <$> (loadData fileName)

  resp <- getTicks "BTC-ADA"
  let ticksV = result $ fromMaybe (APIResponse True "" V.empty) resp
      --fullTicks = history `S.union` (S.fromList . V.toList $ ticksV)
      fullV = ticksV --V.fromList $ S.toAscList fullTicks
      unboxedticks = VU.fromList $ V.toList $ fmap tickToCoords fullV


  let omega = 50

  --history `deepseq` serializeResults fileName $ V.toList fullV

  smooth <- unboxedticks `deepseq` timeIt $ do
    let smoothed = VU.zipWith reconcile unboxedticks (gaussianSmooth omega $ VU.map snd unboxedticks)
    return $! smoothed `deepseq` smoothed

  --flip mapM_ [2,4..70] (\omega -> flip mapM_ [0.01,0.015..0.05] (\earnMin -> do
  let simulationPerf :: (Double, [History])
      simulationPerf = mapFst fromRational $ simulate omega (Budget 1 0 1e12 0) (VU.map snd unboxedticks) (holdAfterPeakBot 0.005 0.01 30)
      coords = VU.map fst unboxedticks
      --smooths = take 1000 $ map (VU.zip coords) $ snd simulationPerf
      isSale (History Sell _) = True
      isSale _ = False
      (sales, buys) = partition isSale . snd $ simulationPerf
      (sales', buys') = (map (pointToCoords . histPoint) sales, map (pointToCoords . histPoint) buys)

      pointToCoords i = unboxedticks VU.! i

  let extrema = findExtrema $ gaussianSmooth omega $ VU.map snd unboxedticks
      extremaCoords = map (\(Extremum _ i) -> smooth VU.! i) extrema

  putStrLn (show $ fst simulationPerf) --))

  timeIt $ toFile (def & fo_size .~ (5000, 5000)) "plot.png" $ do
    plot (line "original" [VU.toList unboxedticks])

    plot $ liftEC $ do
      plot_lines_title .= "smoothed"
      plot_lines_style .= (def {_line_width = 4.0, _line_color = withOpacity black 1})
      plot_lines_values .= [VU.toList smooth]
      plot_lines_limit_values .= []

    plot $ liftEC $ do
      plot_points_title .= "extrema"
      plot_points_style .= (def {_point_radius = 8.0})
      plot_points_values .= extremaCoords

    plot $ liftEC $ do
      plot_points_title .= "sales"
      plot_points_style .= (def {_point_radius = 10.0, _point_color = opaque red})
      plot_points_values .= sales'

    plot $ liftEC $ do
      plot_points_title .= "buys"
      plot_points_style .= (def {_point_radius = 8.0, _point_color = opaque green})
      plot_points_values .= buys'
