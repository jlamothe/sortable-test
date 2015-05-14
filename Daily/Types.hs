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

module Daily.Types where

import Data.Map (Map)
import Data.Time.Calendar (Day)

type ProcessedData = Map Day ProcessedRecord

type ProcessedRecord = Map String ProcessedColumn

data ProcessedColumn =
  ProcessedColumn { colSum    :: Double
                  , colMax    :: Double
                  , colMin    :: Double
                  , colAvg    :: Double
                  , colStdDev :: Double
                  } deriving (Eq, Show)

-- jl
