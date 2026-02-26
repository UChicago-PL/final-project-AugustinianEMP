module BioParser where

import Control.Monad.Except
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- this is where I define ADTs matching the variables within the given txt file

-- Represents an RQ or CT Mean value. It is either a number or empty/undetermined.
data LabValue = Value Double | Empty
    deriving (Show, Eq)

-- A single row of data extracted from the CSV
data LabRow = LabRow 
    { treatment :: String   -- Sample Name
    , primer    :: String   -- Target Name
    , rq        :: LabValue -- The RQ column
    , ctMean    :: LabValue -- The Ct Mean column
    } deriving (Show, Eq)

-- final, validated group of 3 replicates 
data ValidatedGroup = ValidatedGroup
    { vTreatment :: String
    , vPrimer    :: String
    , replicate1 :: Double
    , replicate2 :: Double
    , replicate3 :: Double
    , groupCTMean:: Double
    } deriving (Show, Eq)