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

module Common.Tests (buildTests) where

import Common
import Common.Types
import Data.Time.Calendar (Day, fromGregorian)
import Test.HUnit (Test (..), (@=?))
import Text.CSV (CSV, Record)

buildTests :: Test
buildTests = TestLabel "Common.build" $
  TestList [ blankTableTest
           , emptyTableTest
           , zeroRecordTest
           , buildWithDataTest
           ]

blankTableTest :: Test
blankTableTest = tableTest "blank table" emptyInputData []

emptyTableTest :: Test
emptyTableTest = tableTest "empty table (with headers)" emptyInputData [colBase]

zeroRecordTest :: Test
zeroRecordTest = tableTest "empty table (with columns)" zeroRecordResult [colBase ++ testCols]

buildWithDataTest = tableTest "table with values" dataResult dataInput

tableTest :: String -> InputData -> CSV -> Test
tableTest label result input =
  TestLabel label $
  TestCase $ result @=? build input

zeroRecordResult :: InputData
zeroRecordResult = emptyInputData { inputFields = testCols }

colBase :: Record
colBase = ["Date", "Order"]

testCols :: Record
testCols = ["foo", "bar", "baz"]

dataResult :: InputData
dataResult = emptyInputData { inputFields = testCols
                            , inputRecords = resultRecords
                            }

dataInput :: CSV
dataInput = [ colBase ++ testCols
            , [day1string, "foo", "1", "2.0", "3"]
            , [day1string, "bar", "4", "5.0", "6"]
            , [day2string, "foo", "6", "5.0", "4"]
            , [day2string, "bar", "3", "2.0", "1"]
            ]

resultRecords :: [InputRecord]
resultRecords = [ InputRecord day1 "foo" [1, 2, 3]
                , InputRecord day1 "bar" [4, 5, 6]
                , InputRecord day2 "foo" [6, 5, 4]
                , InputRecord day2 "bar" [3, 2, 1]
                ]

day1string :: String
day1string = "1/1/1970"

day2string :: String
day2string = "1/2/1970"

day1 :: Day
day1 = fromGregorian 1970 1 1

day2 :: Day
day2 = fromGregorian 1970 1 2

-- jl
