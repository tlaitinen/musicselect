module MusicSelect.Solve where

import MusicSelect.Types
import MusicSelect.Simplify
import Data.LinearProgram
import Control.Monad.LPMonad


mpVar :: MusicPieceId -> String
mpVar mpId = "m" ++ show mpId

inGenreVar :: MusicGenreId -> String
inGenreVar mgId = "in" ++ show mgId

missingVar :: MusicGenreId -> String
missingVar mgId = "missing" ++ show mgId

avoidVar :: String
avoidVar = "avoid"

objFun :: Requirements -> LinFunc String Double
objFun reqs = linCombination $ [ 
        (-100000, avoidVar)
    ]

mpSelectLp :: Requirements -> LP String Double
mpSelectLp reqs = execLPM $ do
    setDirection Max 
    setObjective (objFun reqs)


solve :: Requirements -> IO Result 
solve reqs = do
    return $ Result {
        resToAdd = [],
        resToRemove = [],
        resMissing = []
    }
    where
        (startDate, endDate) = reqPeriod reqs
        channelFormats = [ 
                integrateMusicFormats $ combineWeeklyAndOnceTime
                                                (chWeeklyFormats ch) 
                                                (chOnceFormats ch)
                                                startDate 
                                                endDate
                | ch <- reqChannels reqs 
            ]
        
