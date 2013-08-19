module Note where

-- |An ABC notation note.
data ABC = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Enum, Eq, Ord, Read, Show)

type Octave = Integer

-- |Absolute note.
data Note = Note ABC Octave
    deriving (Eq, Read, Show)

instance Ord Note where
    (Note n1 o1) `compare` (Note n2 o2) = (o1, n1) `compare` (o2, n2)

semitonesInOctave = 12

-- |Returns a semitone number for a given note, counting from C.
semitone :: ABC -> Integer
semitone C = 0
semitone Cs = 1
semitone D = 2
semitone Ds = 3
semitone E = 4
semitone F = 5
semitone Fs = 6
semitone G = 7
semitone Gs = 8
semitone A = 9
semitone As = 10
semitone B = 11

-- |Calculates distance between two notes in semitones.
semitoneDistance :: Note -> Note -> Integer
semitoneDistance (Note n1 o1) (Note n2 o2) =
    (o2 - o1) * semitonesInOctave + (semitone n2 - semitone n1)