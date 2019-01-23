{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import qualified Data.Map as M
import qualified Data.Vector as V

data Stop = Stop
  { stopID :: B.ByteString
  , stopName :: B.ByteString
  } deriving (Show)

instance FromNamedRecord Stop where
  parseNamedRecord r = Stop <$> r .: "stop_id" <*> r .: "stop_name"

data StopTimes = StopTimes
  { stTripID :: B.ByteString
  , stArrivalTime :: B.ByteString
  , stDepartureTime :: B.ByteString
  , stStopID :: B.ByteString
  , stStopSequence :: B.ByteString
  } deriving (Show)

instance FromNamedRecord StopTimes where
  parseNamedRecord r =
    StopTimes <$> r .: "trip_id" <*> r .: "arrival_time" <*>
    r .: "departure_time" <*>
    r .: "stop_id" <*>
    r .: "stop_sequence"

-- Parsing

byteOrderMark = B.pack [0xef, 0xbb, 0xbf]

loadGtfsFile :: FromNamedRecord a => String -> String -> IO (V.Vector a)
loadGtfsFile path file = do
  stopsSelector <- Zip.mkEntrySelector file
  content <- Zip.withArchive path (Zip.getEntry stopsSelector)
  let stripedContent =
        if B.isPrefixOf byteOrderMark content
          then B.drop 3 content
          else content
  let bsLazyStops = BSL.fromStrict stripedContent
  case decodeByName bsLazyStops of
    Left err -> fail err
    Right (_, v) -> return v

loadStops :: String -> IO (V.Vector Stop)
loadStops path = loadGtfsFile path "stops.txt"

loadStopTimes :: String -> IO (V.Vector StopTimes)
loadStopTimes path = loadGtfsFile path "stop_times.txt"

main :: IO ()
main = do
  let gtfsFile = "gtfs-sbb-reduced.zip"
  stops <- loadStops gtfsFile
  putStrLn "Done reading stops"
  stopTimes <- loadStopTimes gtfsFile
  putStrLn "Done reading stop_times"
  putStrLn $ show $ length stops
  putStrLn $ show $ length stopTimes

