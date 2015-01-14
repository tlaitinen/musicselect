

import MusicSelect.Types
import MusicSelect.Solve
import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.TimeOfDaySlice
import FuzzyTimings.SlicedTime
import qualified Data.Map as Map
import Data.Time

main :: IO ()
main = do
    r <- solve reqs
    print r
    where
        reqs = Requirements {
            reqChannels = [
                ChannelReqs {
                    chId = 1,
                    chMusicCount = 20,
                    chWeeklyFormats = fromTimeOfDaySlices [
                        (1, [ 
                                mkTimeOfDaySlice (TimeOfDay 0 0 0)
                                                 86400
                                                 (Map.fromList [ (2,1),
                                                                 (3,1) ])
                        ]),
                        (2, [
                               mkTimeOfDaySlice (TimeOfDay 0 0 0)
                                                 86400
                                                 (Map.fromList [ (10,19) ])
                       

                        ])
                    ],
                    chOnceFormats = fromTimeSlices []
                }
            ],
            reqMusicGenres = [
               MusicGenre {
                    mgId = 2,
                    mgPieces = [ 2,4..50 ],
                    mgSmall = False
                },
                MusicGenre {
                    mgId = 3,
                    mgPieces = [ 3,6..50 ],
                    mgSmall = False
                },
                MusicGenre {
                    mgId = 10,
                    mgPieces = [ 10,20..50 ],
                    mgSmall = False
                }
            ],
            reqCurrentMusic = [1] ++ [12,24..50],
            reqBannedMusic = [],
            reqAvoidMusic = [20,40],
            reqPeriod = (fromGregorian 2014 1 6, fromGregorian 2014 1 7)
        }
