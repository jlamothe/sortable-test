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

module Daily (dailySummary, process) where

import Common
import Common.Types
import Daily.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Calendar (Day)

dailySummary :: String -> String
dailySummary rawInput = encode $ process $ decode rawInput

process :: InputData -> ProcessedData
process input =
  Map.map (buildRecord $ inputFields input) $ sortByDate $ inputRecords input

encode :: ProcessedData -> String
encode = undefined

buildRecord :: [String] -> [InputRecord] -> ProcessedRecord
buildRecord fields =
  Map.fromList . zip fields . buildColumns

sortByDate :: [InputRecord] -> Map Day [InputRecord]
sortByDate = foldr addToMap Map.empty

addToMap :: InputRecord -> Map Day [InputRecord] -> Map Day [InputRecord]
addToMap x s = Map.insert day (x : Map.findWithDefault [] day s) s
  where day = inputRecordDate x

buildColumns :: [InputRecord] -> [ProcessedColumn]
buildColumns = undefined

-- jl
