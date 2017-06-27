module Analyses
( runAll
) where

import qualified Analyses.Injection       as AI
import qualified Analyses.ObjectReference as AOR
import qualified Analyses.Xss             as AX
import           Data.Incident
import           Data.Log
import           Data.Maybe               (isJust)
import           Prelude                  hiding (log)

analyses :: [Log -> Maybe Incident]
analyses =  [ AX.analyse
            , AI.analyse
            , AOR.analyse
            ]

runAll :: Log -> [Maybe Incident]
runAll a = filter isJust $ map (\x -> x a) analyses
