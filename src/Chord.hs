module Chord where

import Interval
import Note

-- |A chord is defined by it's root note and all it's
-- intervals, relative to the root note.
data Chord = Chord Note [Interval]
    deriving (Eq, Read, Show)

-- |Converts a chord to a list of notes that constitute the
-- chord.
toNotes :: Chord -> [Note]
toNotes (Chord root intervals) =
    root : (map (root .+) intervals)