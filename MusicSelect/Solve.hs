module MusicSelect.Solve where

import MusicSelect.Types
import MusicSelect.Simplify
import Data.LinearProgram
import Control.Monad.LPMonad
import Control.Monad
import qualified Data.Map as Map


mpVar :: MusicPieceId -> String
mpVar mpId = "m" ++ show mpId

inGenreVar :: MusicGenreId -> String
inGenreVar mgId = "in" ++ show mgId

missingVar :: MusicGenreId -> String
missingVar mgId = "missing" ++ show mgId

avoidVar :: String
avoidVar = "avoid"

objFun :: MusicCounts -> LinFunc String Double
objFun counts = do
    
    linCombination $ [ 
            (-1, avoidVar)
        ]

solve :: Requirements -> IO Result 
solve reqs = do
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
        lp = execLPM $ do
            setDirection Max
            setObjective (objFun musicCounts)
            forM_ (reqMusicGenres reqs) $ \mg -> do
                equal (var (inGenreVar $ mgId mg)) $ 
                    varSum [ mpVar mpId | mpId <- mgPieces mg ]
                                            
                
        (startDate, endDate) = reqPeriod reqs
        musicCounts = Map.unionsWith max channelMusicCounts
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
