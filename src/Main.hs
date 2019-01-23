{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import Control.Exception.Base (SomeException)
import qualified Data.ByteString as B
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import qualified Data.Map as M
import qualified Data.Vector as V

data Stop = Stop
  { stopID :: B.ByteString
  , stopName :: B.ByteString
  -- , stopLat :: B.ByteString
  -- , stopLon :: B.ByteString
  -- , location :: B.ByteString
  -- , parent :: B.ByteString
  } deriving (Show)

instance FromNamedRecord Stop where
  parseNamedRecord r = Stop <$> r .: "stop_id" <*> r .: "stop_name"
      -- <*> r .: "stop_lat"
      -- <*> r .: "stop_lon"
      -- <*> r .: "location_type"
      -- <*> r .: "parent_station"

instance ToNamedRecord Stop where
  toNamedRecord s = namedRecord []

data StopTimes = StopTimes
  { stTripID :: !String
  , stArrivalTime :: !String
  , stDepartureTime :: !String
  , stStopID :: !String
  , stStopSequence :: !String
  } deriving (Show)

-- loadGtfsFile :: FromNamedRecord a => String -> String -> IO (V.Vector a)
-- loadGtfsFile path file = do
--   stopsSelector <- Zip.mkEntrySelector file
--   stops <- Zip.withArchive path (Zip.getEntry stopsSelector)
--   let bsLazyStops = BSL.fromStrict stops
--   putStrLn $ show $ BSL.take 512 bsLazyStops
--   case decodeByName bsLazyStops of
--     Left err -> fail err
--     Right (_, v) -> return v
-- loadStops :: String -> IO (V.Vector Stop)
-- loadStops path = loadGtfsFile path "stops.txt"
-- loadStopTimes :: String -> IO (V.Vector StopTimes)
-- loadStopTimes path = loadGtfsFile path "stop_times.txt"
--
byteOrderMark = B.pack [0xef, 0xbb, 0xbf]

main :: IO ()
main = do
  content <- B.readFile "stops.txt"
  let stripedContent =
        if B.isPrefixOf byteOrderMark content
          then B.drop 3 content
          else content
  let res =
        decodeCSV defCSVSettings stripedContent :: Either SomeException (V.Vector (Named Stop))
  case res of
    Left e -> putStrLn $ show e
    Right r -> do
      putStrLn "success"
      putStrLn $ show r
