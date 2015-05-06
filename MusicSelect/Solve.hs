{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module MusicSelect.Solve where

import MusicSelect.Types
import Data.Maybe
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



objFun :: [MusicGenreId] -> LinFunc String Double
objFun mgIds = do
    
    linCombination $ concat [ 
            [(100000000, genreOkVar mgId') | mgId' <- mgIds ],
            [(-10000, avoidVar)],
            [(100, genreExtraVar mgId') | mgId' <- mgIds ],
            [(1, currentVar)]
        ]

setBounds :: forall (m :: * -> *) v c.
            (Ord c, Ord v, MonadState (LP v c) m) =>
            v -> c -> c -> m ()
setBounds v l u    | l == u = varEq v l
    | otherwise = varBds v l u

solve :: Requirements -> IO Result 
solve reqs = do
    res <- if null (constraints lp)
        then return Nothing
        else do
            (_, mresult) <- glpSolveVars mipDefaults lp
            let res = mresult >>= Just . mkResult . snd
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
            setObjective (objFun $ Map.keys musicCounts)
            -- music piece can be selected=1 or not=0
            forM_ allMusic $ \mpId -> setVarKind (mpVar mpId) BinVar
            -- number of selected music pieces must be 
            -- less than the sum of total music count
            setVarKind selectedVar IntVar
            equal (var selectedVar) $ varSum $ map mpVar allMusic
            setBounds selectedVar 0 $ fromIntegral totalCount

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
            -- indicator variable for when music genre is ok,
            -- and variable to track the number of extra music pieces in a 
            -- genre
            forM_ (Map.toList musicCounts) $ \(mgId',c) -> do
                setVarKind (genreOkVar mgId') ContVar
                setBounds (genreOkVar mgId') 0 1
                leqTo (linCombination [ (-1 / fromIntegral c, inGenreVar mgId'), 
                                      (1, genreOkVar mgId') ]) 0.0
                setVarKind (genreExtraVar mgId') IntVar
                setBounds (genreExtraVar mgId') (fromIntegral $ -c) $ fromIntegral totalMusicCount
                equal (var (genreExtraVar mgId')) $
                    linCombination [ (1, inGenreVar mgId'), 
                                     (fromIntegral $ -c, genreOkVar mgId') ]
                    
        totalCount = sum $ Map.elems musicCounts
        allMusic = map head $ L.group $ L.sort $ concatMap mgPieces $ reqMusicGenres reqs        
        totalMusicCount = length allMusic
        banned = Set.fromList $ reqBannedMusic reqs
        current = Set.fromList $ reqCurrentMusic reqs
        musicCounts = Map.fromList $ reqCounts reqs
        genreSize mgId' = fromMaybe 0 $ listToMaybe [ length $ mgPieces mg |
                                                      mg <- reqMusicGenres reqs,
                                                      mgId mg == mgId' ]
        mkResult :: Map.Map String Double -> Result    
        mkResult vm = Result {
                resToAdd = [ 
                        mpId | mpId <- allMusic, 
                               (floor $ Map.findWithDefault 0 (mpVar mpId) vm) == 1,
                               mpId `Set.notMember` current
                    ],
                resToRemove = [
                        mpId | mpId <- reqCurrentMusic reqs,
                               (floor $ Map.findWithDefault 0 (mpVar mpId) vm) == 0
                    ],
                resMissing = [
                    (mgId', c - (floor $ Map.findWithDefault 0 (inGenreVar mgId') vm))
                        | (mgId',c) <- Map.toList musicCounts,
                          floor (Map.findWithDefault 0 (inGenreVar mgId') vm) < c,
                          not $ maybe False mgSmall $ 
                              L.find ((==mgId') . mgId) $ reqMusicGenres reqs
                ]
            }
