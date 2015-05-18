-- Sortable Test
-- Copyright (C) 2015 Jonathan Lamothe
-- <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

module Daily (dailySummary, process, encode, headersFor) where

import Common
import Common.Types
import Daily.Types
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Calendar (Day)
import Text.CSV (CSV, Record, Field, printCSV)

dailySummary :: String -> String
dailySummary rawInput = printCSV $ encode $ process $ decode rawInput

process :: InputData -> ProcessedData
process input =
  ProcessedData (inputFields input) $ Map.map buildRecord $ sortByDate $ inputRecords input

encode :: ProcessedData -> CSV
encode input = encodeHeader input : encodeBody input

buildRecord :: [InputRecord] -> ProcessedRecord
buildRecord = map buildStats . transpose . map inputRecordValues

buildStats :: [Double] -> Stats
buildStats xs =
  Stats { statSum = sum xs
        , statMax = max' xs
        , statMin = min' xs
        , statAvg = avg xs
        , statStdDev = stdDev xs
        }

max' :: [Double] -> Double
max' (x : xs) = foldl max x xs

min' :: [Double] -> Double
min' (x : xs) = foldl min x xs

avg :: [Double] -> Double
avg xs = sum xs / (fromIntegral . length) xs

stdDev :: [Double] -> Double
stdDev xs = sqrt (sum $ map (\x -> (x - avg xs) ^ 2) xs) / (fromIntegral . length) xs

sortByDate :: [InputRecord] -> Map Day [InputRecord]
sortByDate = foldr addToMap Map.empty

addToMap :: InputRecord -> Map Day [InputRecord] -> Map Day [InputRecord]
addToMap x s = Map.insert day (x : Map.findWithDefault [] day s) s
  where day = inputRecordDate x

encodeHeader :: ProcessedData -> Record
encodeHeader input = "Date" : concatMap headersFor fields
  where fields = processedFields input

encodeBody :: ProcessedData -> CSV
encodeBody input = map encodeRecord recordList
  where recordList = Map.toList $ processedRecords input

headersFor :: Field -> Record
headersFor field = [ "Total " ++ field
                 , "Max " ++ field
                 , "Min " ++ field
                 , "Avg " ++ field
                 , field ++ " Std Dev"
                 ]

encodeRecord :: (Day, ProcessedRecord) -> Record
encodeRecord (day, record) =
  show day : concatMap encodeStats record

encodeStats :: Stats -> Record
encodeStats stats =
  map show [ statSum stats
           , statMax stats
           , statMin stats
           , statAvg stats
           , statStdDev stats
           ]

-- jl
