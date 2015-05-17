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
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Time.Calendar (Day, fromGregorian)
import Test.HUnit (Test (..), assertBool, (@=?))

tests :: Test
tests = TestLabel "Daily.process tests" $
  TestList [ recordCountTest
           , fieldTest
           , recordTests
           ]

recordCountTest :: Test
recordCountTest = TestLabel "record count" $
  TestCase $ Map.size (processedRecords expected)
  @=? Map.size (processedRecords actual)

fieldTest :: Test
fieldTest = TestLabel "field count" $
  TestCase $ fields @=? processedFields actual

recordTests :: Test
recordTests = TestLabel "record tests" $
  TestList $ map recordTests' actualRecords
  where actualRecords = Map.keys $ processedRecords actual

recordTests' :: Day -> Test
recordTests' day =
  TestLabel ("record for " ++ show day) $
  TestList [ statCountTest expectedRecord actualRecord
           , statTests expectedRecord actualRecord
           ]
  where
    expectedRecord = processedRecords expected ! day
    actualRecord = processedRecords actual ! day

statCountTest :: ProcessedRecord -> ProcessedRecord -> Test
statCountTest expected actual =
  TestLabel "value counts" $
  TestCase $ length expected @=? length actual

statTests :: ProcessedRecord -> ProcessedRecord -> Test
statTests expected actual =
  TestLabel "stat tests" $
  TestList $ map statTests' $ zip3 fields expected actual

statTests' :: (String, Stats, Stats) -> Test
statTests' (label, expected, actual) =
  TestLabel label $
  TestList $ map (statTest expected actual)
  [ ("statSum", statSum)
  , ("statMax", statMax)
  , ("statMin", statMin)
  , ("statAvg", statAvg)
  , ("statStdDev", statStdDev)
  ]

statTest :: Stats -> Stats -> (String, Stats -> Double) -> Test
statTest expected actual (label, f) =
  TestLabel label $
  TestCase $ assertBool errMsg $
  isClose expectedVal actualVal
  where
    errMsg = "expected: " ++ show expectedVal ++ " got: " ++ show actualVal
    expectedVal = f expected
    actualVal = f actual

isClose :: Double -> Double -> Bool
isClose x y =
  if x == 0 || y == 0
  then abs (x - y) < delta
  else abs (1 - x / y) < delta
  where delta = 0.0001

input :: InputData
input =
  emptyInputData { inputFields = fields
                 , inputRecords = records
                 }

fields :: [String]
fields = map (\x -> "column " ++ show x) [1..3]

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
day1record = [day1col1, day1col2, day1col3]

day2record :: ProcessedRecord
day2record = [day2col1, day2col2, day2col3]

day1col1 :: Stats
day1col1 = buildStats 1 4

day1col2 :: Stats
day1col2 = buildStats 2 5

day1col3 :: Stats
day1col3 = buildStats 3 6

day2col1 :: Stats
day2col1 = buildStats 6 3

day2col2 :: Stats
day2col2 = buildStats 5 2

day2col3 :: Stats
day2col3 = buildStats 4 1

buildStats :: Double -> Double -> Stats
buildStats x y =
  Stats { statSum = x + y
        , statMax = max x y
        , statMin = min x y
        , statAvg = avg x y
        , statStdDev = stdDev x y
        }

avg :: Double -> Double -> Double
avg x y = (x + y) / 2

stdDev :: Double -> Double -> Double
stdDev x y = sqrt ((x - avg x y) ^ 2 + (y - avg x y) ^ 2) / 2

-- jl
