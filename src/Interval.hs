module Interval where

newtype Interval = Interval Integer
    deriving (Eq, Ord, Read, Show)

perf1 = Interval 0
dim2 = perf1

min2 = Interval 1
aug1 = min2

maj2 = Interval 2
dim3 = maj2

min3 = Interval 3
aug2 = min3

maj3 = Interval 4
dim4 = maj3

perf4 = Interval 5
aug3 = perf4

dim5 = Interval 6
aug4 = dim5

perf5 = Interval 7
dim6 = perf5

min6 = Interval 8
aug5 = min6

maj6 = Interval 9
dim7 = maj6

min7 = Interval 10
aug6 = min7

maj7 = Interval 11
dim8 = maj7

perf8 = Interval 12
aug7 = perf8

unison = perf1
semitone = min2
tone = maj2
tritone = dim5

inversion :: Interval -> Interval
inversion (Interval i) = Interval . abs $ i - 12
