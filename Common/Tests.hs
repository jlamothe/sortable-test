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
import Test.HUnit (Test (..), (@=?))
import Text.CSV (CSV, Record)

buildTests :: Test
buildTests = TestLabel "Common.build" $
  TestList [ blankTableTest
           , emptyTableTest
           , zeroRecordTest
           ]

blankTableTest :: Test
blankTableTest = tableTest "blank table" emptyInputData []

emptyTableTest :: Test
emptyTableTest = tableTest "empty table (with headers)" emptyInputData [colBase]

zeroRecordTest :: Test
zeroRecordTest = tableTest "empty table (with columns)" zeroRecordResult [colBase ++ testCols]

tableTest :: String -> InputData -> CSV -> Test
tableTest label result input =
  TestLabel label $
  TestCase $ result @=? build input

zeroRecordResult :: InputData
zeroRecordResult = emptyInputData { fields = testCols }

colBase :: Record
colBase = ["Date", "Order"]

testCols :: Record
testCols = ["foo", "bar", "baz"]

-- jl
