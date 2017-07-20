module Analyses
( runAll
) where

import qualified Analyses.Injection          as AI
import qualified Analyses.ObjectReference    as AOR
import qualified Analyses.Xss                as AX
import           Control.Monad               (join)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Incident
import           Data.Log
import           Data.Maybe                  (fromJust, isJust)
import           Prelude                     hiding (log)


analyses :: [Log -> Maybe Incident]
analyses =  [ AX.analyse
            , AI.analyse
            , AOR.analyse
            ]

runAll :: [Log] -> [Incident]
runAll a = map fromJust $ filter isJust $ join $ filter ([] /=) $ parMap rdeepseq testLog a

testLog :: Log -> [Maybe Incident]
testLog a = filter isJust $ map (\x -> x a) analyses
