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
    let lp = mpSelectLp reqs
    print lp
    res <- if null (constraints lp)
        then return Nothing
        else do
            (_, mresult) <- glpSolveVars mipDefaults lp
            print mresult
            let res = (mresult >>= \(_,vm) -> return $ mkResult vm)
            print res
            return res 
    case res of
        Just r -> return r
        Nothing -> return $ Result {
                resToAdd = [],
                resToRemove = [],
                resMissing = []
            }   
            
    where
        (startDate, endDate) = reqPeriod reqs
        channelMusicCounts = [
                musicFormatToMusicCount (chMusicCount ch) $ 
                    integrateMusicFormats $ combineWeeklyAndOnceTime
                                                (chWeeklyFormats ch) 
                                                (chOnceFormats ch)
                                                startDate 
                                                endDate
                | ch <- reqChannels reqs 
            ]
        mkResult vm = Result {
                resToAdd = [],
                resToRemove = [],
                resMissing = []
            }
