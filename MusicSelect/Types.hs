module MusicSelect.Types where

import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.SlicedTime
import Data.Time

type MusicPieceId = Int
type MusicGenreId = Int
type ChannelId    = Int
type Weight       = Rational

data MusicGenre = MusicGenre {
    mgId     :: MusicGenreId,
    mgPieces :: [MusicPieceId],
    -- | if there are not enough music pieces to allocate from a 
    -- small music genre, then the remaining music pieces may be
    -- allocated to satisfy other requirements without error
    mgSmall  :: Bool
}

data MusicFormat = MusicFormat [(MusicGenreId, Weight)]

data ChannelReqs = ChannelReqs {
    chId            :: ChannelId,
    chMusicCount    :: Int,
    chWeeklyFormats :: WeeklySlicedTime MusicFormat,
    chOnceFormats   :: SlicedTime MusicFormat
}

data Requirements = Requirements {
    reqChannels     :: [ChannelReqs],
    reqMusicGenres  :: [MusicGenre],

    -- | current set of music pieces
    reqCurrentMusic :: [MusicPieceId],

    -- | banned music pieces will not be selected
    reqBannedMusic  :: [MusicPieceId],

    -- | list of music pieces that should be avoided if possible
    reqAvoidMusic   :: [MusicPieceId],

    -- | planning period
    reqPeriod       :: (Day,Day)
}

data Result = Result {
    resToAdd    :: [MusicPieceId],
    resToRemove :: [MusicPieceId],
    resMissing  :: [(MusicGenreId, Int)]
}
