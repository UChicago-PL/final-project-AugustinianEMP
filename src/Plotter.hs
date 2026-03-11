module Plotter where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import BioParser (GroupedResult(..))
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (void)
import System.Directory (createDirectoryIfMissing)

-- for sorting veh so that it always appears on the leftmost side in the plot
treatmentOrder :: String -> (Int, String)
treatmentOrder "Veh" = (0, "Veh")
treatmentOrder other = (1, other)

-- Groups the final data by Primer so we can make one chart per Primer
plotAllPrimers :: String -> [GroupedResult] -> IO ()
plotAllPrimers runName results = do
    
    -- Creates specific folder for THIS dataset
    let runFolder = "plots/" ++ runName
    createDirectoryIfMissing True runFolder
    
    let sortedByPrimer = sortBy (compare `on` gPrimer) results
    let groupedByPrimer = groupBy (\g1 g2 -> gPrimer g1 == gPrimer g2) sortedByPrimer
    
    -- Passes folder name to the chart generator
    mapM_ (generateChart runFolder) groupedByPrimer

-- Generates a single PNG chart for a specific Primer
generateChart :: FilePath -> [GroupedResult] -> IO ()
generateChart _ [] = pure ()
generateChart folder primerGroup = do
    let currentPrimer = gPrimer (head primerGroup)
    
    -- Saves inside the dataset's folder
    let filename = folder ++ "/" ++ currentPrimer ++ "_RQ_Plot.png"
    
    -- Sort the group so "Veh" is first, then alphabetical
    let sortedGroup = sortBy (comparing (treatmentOrder . gTreatment)) primerGroup
    -- Format data using the newly sorted group
    let labels = map gTreatment sortedGroup
    let values = map (\g -> [gAvgRQ g]) sortedGroup
    
    void $ toFile def filename $ do
        layout_title .= "RQ for Primer: " ++ currentPrimer
        layout_legend .= Nothing     
        layout_x_axis . laxis_generate .= autoIndexAxis labels
        layout_y_axis . laxis_title .= "RQ"
        
        plot $ plotBars <$> bars [currentPrimer] (addIndexes values)