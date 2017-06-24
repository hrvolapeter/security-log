module Analyses
( runAll
) where

import qualified Analyses.Xss  as Xss
import           Data.Incident
import           Data.Log

analyses = [ Xss.analyse
           ]

runAll :: Log -> [Maybe Incident]
runAll log = map (\x -> x log) analyses
