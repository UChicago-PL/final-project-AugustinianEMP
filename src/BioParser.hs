module BioParser where

import Control.Monad.Except
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)


-- this is where I define ADTs matching the variables within the given txt file

-- Represents an RQ/CT Mean value. It is either a number or empty/undetermined.
data LabValue = Value Double | Empty
    deriving (Show, Eq)

-- target values
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

data LabError 
    = ParseError String 
    | EmptyData 
    | ThresholdFailed String
    deriving (Show, Eq)

-- fn that helps skip metadata that comes with txt file
skipMetadata :: ReadP ()
-- munch reads characters until it finds the str "Well"
skipMetadata = void $ munch (\_ -> True) *> string "Well"

-- ReadP logic
parseCSVFields :: ReadP [String]
parseCSVFields = sepBy (munch (/= ',')) (char ',')

-- converts raw str into LabValue ADT
toLabValue :: String -> LabValue
toLabValue "" = Empty
toLabValue "Undetermined" = Empty
toLabValue str = case readMaybe str :: Maybe Double of
    Just n  -> Value n
    Nothing -> Empty

-- Processes each row of the CSV and extracts the 4 target values
processLine :: String -> Either String LabRow
processLine rawLine = 
    let line = filter (/= '\r') rawLine
        
        parses = readP_to_S parseCSVFields line

        -- using the Fokker trick
        fullParses = [fs | (fs, "") <- parses]
        
    in case fullParses of
        (fields:_) -> 
            -- check for validity of parsed txt
            if length fields >= 16 
            then Right $ LabRow 
                (fields !! 3)         -- Sample Name             
                (fields !! 4)         -- Target Name        
                (toLabValue (fields !! 11))   -- RQ val
                (toLabValue (fields !! 15))   -- Ct Mean val
            else Right (LabRow "" "" Empty Empty)
            
        -- if all else fails
        [] -> Left $ "Could not parse line: " ++ take 20 line ++ "..."

-- Define a list of primers to exclude from the final plot (aka control genes)
controlGenes :: [String]
controlGenes = ["B2M"]

parseLabExport :: String -> ExceptT LabError IO [LabRow]
parseLabExport rawCSV = do
    let allLines = lines rawCSV
    -- Drops beginning metadata 
    let dataLines = drop 35 allLines 
    
    -- monadic error check: is there still data after passing metadata?
    when (null dataLines) $ throwError EmptyData
    
    let parsedLines = map processLine dataLines
    
    -- makes a list of Row instead a lst of Either Error Row
    case sequence parsedLines of
        Left err -> throwError (ParseError err)
        Right rows -> do
            -- filters out rows with the given control genes (ex: B2M)
            let validRows = filter (\r -> not (primer r `elem` controlGenes) && primer r /= "") rows
            
            liftIO $ putStrLn $ "\n >> Successfully parsed " ++ show (length validRows) ++ " target rows."
            
            pure validRows





