module Analyses
( runAll
) where

import qualified Analyses.Xss as Xss
import           Database     (Log)

analyses = [Xss.analyse]

runAll :: Log -> [Maybe String]
runAll log = map (\x -> x log) analyses
