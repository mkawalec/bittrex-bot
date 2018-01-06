module Lib
    ( someFunc
    , makeAuthRequest
    ) where

import Control.Lens
import Control.Applicative

import Data.Aeson (toJSON)
import Network.Wreq
import Data.HMAC (hmac, HashMethod(..))
import Data.Digest.SHA512 (hash)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Time.Clock.POSIX (getPOSIXTime)

apiSecret :: ByteString
apiSecret = "003dda1db8804f98bf6f4345b1e94dc7"

apiKey :: ByteString
apiKey = "b2767c02a7ca49b4a59004dea535c704"


someFunc :: IO ()
someFunc = putStrLn "someFunc"


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
      opts = defaults & header "apisign" .~ [B.pack signature]
  getWith opts (BC.unpack fullPath)
