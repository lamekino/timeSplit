module Audio where

import Types

type SongLength = (Timestamp, Timestamp)

-- | Convert Timestamp to a raw second value
fromTimestamp :: Timestamp -> Double
fromTimestamp ts = case ts of
  (Hour h m s) -> fromIntegral $ h * 3600 + m * 60 + s
  (Minute m s) -> fromIntegral $ m * 60 + s
  (Second x) -> fromIntegral x
  Beginning -> 0.0

-- | Drop the beginning up to start and drop the end past end
-- splitFile :: Handle -> IO [Handle]
-- splitFile = undefined
