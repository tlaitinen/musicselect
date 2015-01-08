module MusicSelect.Simplify where
import MusicSelect.Types
import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.SlicedTime
import FuzzyTimings.TimeSlice
import Data.Time
import qualified Data.Map as Map
import Data.List

combineWeeklyAndOnceTime :: WeeklySlicedTime a -> SlicedTime a -> Day -> Day -> SlicedTime a
combineWeeklyAndOnceTime wst st2 startDay endDay = flattenSlicedTime st3
    where
        st1 = implementWeeklySlicedTime wst startDay endDay
        st1WoSt2 = st1 `deleteSlicedTime` st2
        st3 = fromTimeSlices $ toTimeSlices st1WoSt2 ++ toTimeSlices st2

integrateMusicFormats :: SlicedTime MusicFormat -> MusicFormat
integrateMusicFormats st = foldl' combine Map.empty mfs
    where
       mfs = [ mkFormat ts | ts <- toTimeSlices st ]
       mkFormat ts = Map.map (* (fromIntegral $ tsDuration ts)) $ tsValue ts 
       combine mf1 mf2 = Map.unionWith (+) mf1 mf2

musicFormatToMusicCount :: Int -> MusicFormat -> MusicCounts       
musicFormatToMusicCount total mf = Map.fromList $ f total (Map.toList mf) []
    where
        totalWeights = Map.fold (+) 0 mf
        f r ((mgId, w):xs) ys = let c = w / totalWeights * (fromIntegral total)
                                in  f (r-floor c) xs ((mgId,floor c,c):ys)
        f r [] ((mgId,cf,c):ys) 
            | r > 0 && c > fromIntegral cf = (mgId, cf+1):f (r-1) [] ys
            | otherwise = (mgId,cf):f r [] ys
        f _ [] [] = []
                                    
