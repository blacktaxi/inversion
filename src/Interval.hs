module Interval where

newtype Interval = Interval Integer
    deriving (Eq, Ord, Read, Show)

instance Num Interval where
    fromInteger = Interval

    (Interval i1) + (Interval i2) = Interval (i1 + i2)
    (Interval i1) - (Interval i2) = Interval (i1 - i2)
    abs (Interval i) = Interval . abs $ i
    signum (Interval i) = Interval . signum $ i

    -- When does it make sense to multiply intervals?
    (*) = undefined

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
octave = perf8

inversion :: Interval -> Interval
inversion i = abs (i - octave)