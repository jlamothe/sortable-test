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

module Daily.Tests.Process (tests) where

import Common.Types
import Daily (process)
import Daily.Types
import qualified Data.Map as Map
import Data.Time.Calendar (Day, fromGregorian)
import Test.HUnit (Test (..), (@=?))

tests :: Test
tests = TestLabel "Daily.process tests" $
  TestList [ recordCountTest
           , fieldTest
           , valueCountTests
           ]

recordCountTest :: Test
recordCountTest = TestLabel "record count" $
  TestCase $ Map.size (processedRecords expected)
  @=? Map.size (processedRecords actual)

fieldTest :: Test
fieldTest = TestLabel "field count" $
  TestCase $ fields @=? processedFields actual

valueCountTests :: Test
valueCountTests = TestLabel "value counts" $
  TestList $ map valueCountTest (Map.toList actualRecords)
  where actualRecords = processedRecords actual

valueCountTest :: (Day, ProcessedRecord) -> Test
valueCountTest (day, record) =
  TestLabel ("for day " ++ show day) $
  TestCase $ length fields @=? length record

input :: InputData
input =
  emptyInputData { inputFields = fields
                 , inputRecords = records
                 }

fields :: [String]
fields = ["foo", "bar", "baz"]

records :: [InputRecord]
records = [ InputRecord day1 "foo" [1, 2, 3]
          , InputRecord day1 "bar" [4, 5, 6]
          , InputRecord day2 "foo" [6, 5, 4]
          , InputRecord day2 "bar" [3, 2, 1]
          ]

expected :: ProcessedData
expected = ProcessedData fields $
  Map.fromList [ (day1, day1record)
               , (day2, day2record)
               ]

actual :: ProcessedData
actual = process input

day1 :: Day
day1 = fromGregorian 1970 1 1

day2 :: Day
day2 = fromGregorian 1970 1 2

day1record :: ProcessedRecord
day1record = [day1foo, day1bar, day1baz]

day2record :: ProcessedRecord
day2record = [day2foo, day2bar, day2baz]

day1foo :: ProcessedValues
day1foo = processedColumn 1 4

day1bar :: ProcessedValues
day1bar = processedColumn 2 5

day1baz :: ProcessedValues
day1baz = processedColumn 3 6

day2foo :: ProcessedValues
day2foo = processedColumn 6 3

day2bar :: ProcessedValues
day2bar = processedColumn 5 2

day2baz :: ProcessedValues
day2baz = processedColumn 4 1

processedColumn :: Double -> Double -> ProcessedValues
processedColumn x y =
  ProcessedValues (x + y) (max x y) (min x y) (avg x y) (stdDev x y)

avg :: Double -> Double -> Double
avg x y = x + y / 2

stdDev :: Double -> Double -> Double
stdDev x y = sqrt ((x - avg x y) ^ 2 + (y - avg x y) ^ 2) / 2
