{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module MusicSelect.Solve where

import MusicSelect.Types
import MusicSelect.Simplify
import Data.LinearProgram
import Control.Monad.LPMonad
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import qualified Data.List as L


mpVar :: MusicPieceId -> String
mpVar mpId = "m" ++ show mpId

inGenreVar :: MusicGenreId -> String
inGenreVar mgId = "in" ++ show mgId

genreOkVar :: MusicGenreId -> String
genreOkVar mgId = "ok" ++ show mgId

genreExtraVar :: MusicGenreId -> String
genreExtraVar mgId = "extra" ++ show mgId

missingVar :: MusicGenreId -> String
missingVar mgId = "missing" ++ show mgId

selectedVar :: String
selectedVar = "selected"

currentVar :: String
currentVar = "current"

avoidVar :: String
avoidVar = "avoid"



objFun :: MusicCounts -> LinFunc String Int
objFun counts = do
    
    linCombination $ [ 
            (-1, avoidVar)
        ]

setBounds :: forall (m :: * -> *) v c.
            (Ord c, Ord v, MonadState (LP v c) m) =>
            v -> c -> c -> m ()
setBounds v l u    | l == u = varEq v l
    | otherwise = varBds v l u

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
            -- music piece can be selected=1 or not=0
            forM_ allMusic $ \mpId -> do
                setVarKind (mpVar mpId) IntVar
                setBounds (mpVar mpId) 0 1
            -- number of selected music pieces must be 
            -- less than the sum of channel music counts
            setVarKind selectedVar IntVar
            equal (var selectedVar) $ varSum $ map mpVar allMusic
            setBounds selectedVar 0 totalCount

            -- number of selected music pieces that are already present
            setVarKind currentVar IntVar
            equal (var currentVar) $ varSum $ [ mpVar mpId | mpId <- allMusic,
                mpId `Set.member` current ]

            -- total number of selected music pieces to avoid
            setVarKind avoidVar IntVar
            equal (var avoidVar) $ varSum $ map mpVar $ reqAvoidMusic reqs
            -- count the number of selected music pieces in each
            -- music genre
            forM_ (reqMusicGenres reqs) $ \mg -> do
                setVarKind (inGenreVar $ mgId mg) IntVar
                equal (var (inGenreVar $ mgId mg)) $ 
                    varSum [ mpVar mpId | mpId <- mgPieces mg,
                             mpId `Set.notMember` banned ]

                setVarKind (genreOkVar $ mgId mg) IntVar
                setBounds (genreOkVar $ mgId mg) 0 1
                -- TODO 
                -- leq (var (genreOkVar $ mgId mg)) $             
        totalCount = sum $ map chMusicCount $ reqChannels reqs
        allMusic = map head $ L.group $ L.sort $ concatMap mgPieces $ reqMusicGenres reqs        
        (startDate, endDate) = reqPeriod reqs
        banned = Set.fromList $ reqBannedMusic reqs
        current = Set.fromList $ reqCurrentMusic reqs
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
