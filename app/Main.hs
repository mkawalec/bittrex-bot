module Main where

import Lib
import Network.Wreq
import Control.Lens

main :: IO ()
main = do
  r <- makeAuthRequest "https://bittrex.com/api/v1.1/public/getmarkets"
  putStrLn "dua"
  --putStrLn . show $ r ^. responseBody
