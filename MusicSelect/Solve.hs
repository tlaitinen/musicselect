module MusicSelect.Solve where

import MusicSelect.Types
import Data.LinearProgram
import Control.Monad.LPMonad




solve :: Requirements -> IO Result 
solve reqs = return $ Result {
        resToAdd = [],
        resToRemove = [],
        resMissing = []
    }
