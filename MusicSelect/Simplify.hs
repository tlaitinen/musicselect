module MusicSelect.Simplify where
import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.SlicedTime
import Data.Time
combineOnceAndWeeklyTime :: WeeklySlicedTime a -> SlicedTime a -> Day -> Day -> SlicedTime a
combineOnceAndWeeklyTime wst st2 startDay endDay = flattenSlicedTime st3
    where
        st1 = implementWeeklySlicedTime wst startDay endDay
        st1WoSt2 = st1 `deleteSlicedTime` st2
        st3 = fromTimeSlices $ toTimeSlices st1WoSt2 ++ toTimeSlices st2
         
