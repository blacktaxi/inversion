module Chord where

import Interval (Interval (..), addInterval)
import Note

-- |A chord is defined by it's root note and all it's
-- intervals, relative to the root note.
data Chord = Chord Note [Interval]
    deriving (Eq, Read, Show)

-- |An interval in a chord spec.
-- @TODO rename fields
data ChordInterval = ChordInterval
    { getInterval :: Interval
    , getIsOptional :: Bool
    , getCanUseMany :: Bool
    , getFlexibleOctave :: Bool
    }
    deriving (Eq, Show, Read)

-- |
data ChordSpec = ChordSpec Note [ChordInterval]
    deriving (Eq, Show, Read)

-- |Converts a chord to a list of notes that constitute the
-- chord.
toNotes :: Chord -> [Note]
toNotes (Chord root intervals) =
    map (root `addInterval`) intervals