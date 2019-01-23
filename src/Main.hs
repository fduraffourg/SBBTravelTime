{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import qualified Data.Map as M
import qualified Data.Vector as V

data Stop = Stop
  { stopID :: !String
  , stopName :: !String
  } deriving (Show)

instance FromNamedRecord Stop where
  parseNamedRecord r = Stop <$> r .: "stop_id" <*> r .: "stop_name"

data StopTimes = StopTimes
  { stTripID :: !String
  , stArrivalTime :: !String
  , stDepartureTime :: !String
  , stStopID :: !String
  , stStopSequence :: !String
  } deriving (Show)

instance FromNamedRecord StopTimes where
  parseNamedRecord r =
    StopTimes <$> r .: "trip_id" <*> r .: "arrival_time" <*>
    r .: "departure_time" <*>
    r .: "stop_id" <*>
    r .: "stop_sequence"

loadGtfsFile :: FromNamedRecord a => String -> String -> IO (V.Vector a)
loadGtfsFile path file = do
  stopsSelector <- Zip.mkEntrySelector file
  stops <- Zip.withArchive path (Zip.getEntry stopsSelector)
  let bsLazyStops = BSL.fromStrict stops
  putStrLn $ show $ BSL.take 512 bsLazyStops
  case decodeByName bsLazyStops of
    Left err -> fail err
    Right (_, v) -> return v

loadStops :: String -> IO (V.Vector Stop)
loadStops path = loadGtfsFile path "stops.txt"

loadStopTimes :: String -> IO (V.Vector StopTimes)
loadStopTimes path = loadGtfsFile path "stop_times.txt"

main :: IO ()
main = do
  let gtfsFile = "sample-feed.zip"
  stops <- loadStops gtfsFile
  putStrLn "Done reading stops"
  stopTimes <- loadStopTimes gtfsFile
  putStrLn "Done reading stop_times"
  putStrLn $ show stops
  putStrLn $ show stopTimes
