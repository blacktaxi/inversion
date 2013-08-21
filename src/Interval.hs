module Interval where

import Note

-- |Musical interval, in semitones.
newtype Interval = Interval Integer
    deriving (Eq, Ord, Read, Show)

-- |Interval arithmetic.
class IntervalNum a where
    (.+) :: a -> Interval -> a
    (.-) :: a -> Interval -> a

instance IntervalNum Interval where
    (Interval i1) .+ (Interval i2) = Interval (i1 + i2)
    (Interval i1) .- (Interval i2) = Interval (i1 - i2)

-- Here we heavily rely on the fact that note and interval
-- arithmetic are very closely related. The 'abstractions'
-- don't seem to hold up.
instance IntervalNum Note where
    (Note n o) .+ (Interval i) =
        (Note newNote newOctave)
        where noteSemitone = toInteger (fromEnum n)
              (d, m) = (noteSemitone + i) `divMod` semitonesInOctave
              newNote = toEnum $ fromInteger m
              newOctave = o + d
    n .- (Interval i) = n .+ (Interval (-i))

-- @TODO doesn't belong to IntervalNum and doesn't belong here
-- either?
(Interval i) .* x = Interval (i * x)

inversion :: Interval -> Interval
inversion (Interval i) = Interval . abs $ i - semitonesInOctave

-- Common intervals
perf1 = Interval 0
min2 = Interval 1
maj2 = Interval 2
min3 = Interval 3
maj3 = Interval 4
perf4 = Interval 5
dim5 = Interval 6
perf5 = Interval 7
min6 = Interval 8
maj6 = Interval 9
min7 = Interval 10
maj7 = Interval 11
perf8 = Interval 12

-- Alternative names
dim2 = perf1
aug1 = min2
dim3 = maj2
aug2 = min3
dim4 = maj3
aug3 = perf4
aug4 = dim5
dim6 = perf5
aug5 = min6
dim7 = maj6
aug6 = min7
dim8 = maj7
aug7 = perf8

-- 'Human' names
unison = perf1
semitone = min2
tone = maj2
tritone = dim5
fourth = perf4
fifth = perf5
octave = perf8