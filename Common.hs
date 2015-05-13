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

module Common (decode, build) where

import Common.Types
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Calendar (Day, fromGregorian)
import Safe (readMay)
import Text.CSV (CSV, Record, parseCSV)

decode :: String -> InputData
decode rawInput = case parseCSV "" rawInput of
  Right csv -> build csv
  _         -> emptyInputData

build :: CSV -> InputData
build csv = emptyInputData { inputFields = getFields csv
                           , inputRecords = getRecords csv
                           }

getFields :: CSV -> [String]
getFields (record : _) = case record of
  (_ : _ : xs) -> xs
  _            -> []
getFields _ = []

getRecords :: CSV -> [InputRecord]
getRecords (_ : records) = mapMaybe toRecord records
getRecords _             = []

toRecord :: Record -> Maybe InputRecord
toRecord (dateString : order : xs) = do
  date <- decodeDate dateString
  Just $ InputRecord date order $ map toVal xs
toRecord _ = Nothing

decodeDate :: String -> Maybe Day
decodeDate str = case splitOn "/" str of
  [mStr, dStr, yStr] -> do
    month <- readMay mStr :: Maybe Int
    day   <- readMay dStr :: Maybe Int
    year  <- readMay yStr :: Maybe Integer
    Just $ fromGregorian year month day
  _ -> Nothing

toVal :: String -> Double
toVal str = fromMaybe 0 $ readMay str

-- jl
