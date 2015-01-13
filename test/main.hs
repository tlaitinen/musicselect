import MusicSelect.Types
import MusicSelect.Solve
import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.TimeOfDaySlice
import FuzzyTimings.SlicedTime
import qualified Data.Map as Map

main :: IO ()
main = do
    r <- solve reqs
    print r
    where
        reqs = Requirements {
            reqChannels = [
                ChannelReqs {
                    chId = 1,
                    chMusicCount = 10,
                    chWeeklyFormats = fromTimeOfDaySlices [
                        (1, [ 
                                mkTimeOfDaySlice (TimeOfDay 0 0 0)
                                                 86400
                                                 (Map.fromList [ (1,1) ])
                        ])
                    ],
                    chOnceFormats = fromTimeSlices []
                }
            ],
            reqMusicGenres = [
                MusicGenre {
                    mgId = 1,
                    mgPieces = [ 1 .. 100 ],
                    mgSmall = False
                },
                MusicGenre {
                    mgId = 2,
                    mgPieces = [ 2,4..100 ],
                    mgSmall = False
                },
                MusicGenre {
                    mgId = 3,
                    mgPieces = [ 3,6..100 ],
                    mgSmall = False
                },
                MusicGenre {
                    mgId = 10,
                    mgPieces = [ 10,20..100 ],
                    mgSmall = True
                }
            ]
        }
