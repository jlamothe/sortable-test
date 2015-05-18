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

module Daily.Tests.Encode (test) where

import Daily
import Daily.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Calendar (Day, fromGregorian)
import Test.HUnit (Test (..), (@=?))
import Text.CSV (CSV, Record)

test :: Test
test = TestLabel "Daily.encode" $
  TestCase $ expected @=? actual

input :: ProcessedData
input =
  ProcessedData { processedFields = fields
                , processedRecords = inputRecords
                }

expected :: CSV
expected = [expectedHeaders, expectedRecord1, expectedRecord2]

actual :: CSV
actual = encode input

fields :: [String]
fields = ["foo", "bar"]

inputRecords :: Map Day ProcessedRecord
inputRecords =
  Map.fromList [ (day1, record1)
               , (day2, record2)
               ]

day1 :: Day
day1 = fromGregorian 1970 1 1

day2 :: Day
day2 = fromGregorian 1970 1 2

record1 :: ProcessedRecord
record1 = [stats1foo, stats1bar]

record2 :: ProcessedRecord
record2 = [stats2foo, stats2bar]

stats1foo :: Stats
stats1foo =
  Stats { statSum = 1
        , statMax = 2
        , statMin = 3
        , statAvg = 4
        , statStdDev = 5
        }

stats1bar :: Stats
stats1bar =
  Stats { statSum = 6
        , statMax = 7
        , statMin = 8
        , statAvg = 9
        , statStdDev = 10
        }

stats2foo :: Stats
stats2foo =
  Stats { statSum = 11
        , statMax = 12
        , statMin = 13
        , statAvg = 14
        , statStdDev = 15
        }

stats2bar :: Stats
stats2bar =
  Stats { statSum = 16
        , statMax = 17
        , statMin = 18
        , statAvg = 19
        , statStdDev = 20
        }

expectedHeaders :: Record
expectedHeaders = "Date" : concatMap headersFor fields

expectedRecord1 :: Record
expectedRecord1 = show day1 : map show ([1..10] :: [Double])

expectedRecord2 :: Record
expectedRecord2 = show day2 : map show ([11..20] :: [Double])

-- jl
