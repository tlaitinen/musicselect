module MusicSelect.Types where

import FuzzyTimings.WeeklySlicedTime
import FuzzyTimings.SlicedTime
import Data.Time
import qualified Data.Map as Map

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

type MusicFormat = Map.Map MusicGenreId Weight
type MusicCounts = Map.Map MusicGenreId Int

data Requirements = Requirements {
    -- number of music pieces to select for each genre
    reqCounts       :: [(MusicGenreId, Int)],

    reqMusicGenres  :: [MusicGenre],

    -- | current set of music pieces
    reqCurrentMusic :: [MusicPieceId],

    -- | banned music pieces will not be selected
    reqBannedMusic  :: [MusicPieceId],

    -- | list of music pieces that should be avoided if possible
    reqAvoidMusic   :: [MusicPieceId]

}

data Result = Result {
    resToAdd    :: [MusicPieceId],
    resToRemove :: [MusicPieceId],
    resMissing  :: [(MusicGenreId, Int)]
} deriving (Show)
