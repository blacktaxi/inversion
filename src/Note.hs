{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Note where

-- |An ABC notation note.
data ABC = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
    deriving (Read, Show)

instance Bounded ABC where
    minBound = C
    maxBound = B

instance Enum ABC where
    fromEnum C = 0
    fromEnum Cs = 1
    fromEnum Db = 1
    fromEnum D = 2
    fromEnum Ds = 3
    fromEnum Eb = 3
    fromEnum E = 4
    fromEnum F = 5
    fromEnum Fs = 6
    fromEnum Gb = 6
    fromEnum G = 7
    fromEnum Gs = 8
    fromEnum Ab = 8
    fromEnum A = 9
    fromEnum As = 10
    fromEnum Bb = 10
    fromEnum B = 11

    toEnum 0 = C
    toEnum 1 = Cs
    toEnum 2 = D
    toEnum 3 = Ds
    toEnum 4 = E
    toEnum 5 = F
    toEnum 6 = Fs
    toEnum 7 = G
    toEnum 8 = Gs
    toEnum 9 = A
    toEnum 10 = As
    toEnum 11 = B

instance Eq ABC where
    x == y = fromEnum x == fromEnum y

instance Ord ABC where
    compare x y = compare (fromEnum x) (fromEnum y)

newtype Octave = Octave Integer
    deriving (Enum, Eq, Ord, Read, Show, Num)

instance Bounded Octave where
    minBound = Octave 0
    maxBound = Octave 8

-- |Absolute note.
data Note = Note ABC Octave
    deriving (Eq, Read, Show)

instance Ord Note where
    (Note n1 o1) `compare` (Note n2 o2) = (o1, n1) `compare` (o2, n2)

semitonesInOctave = toInteger $ fromEnum (maxBound :: ABC) - fromEnum (minBound :: ABC) + 1

-- |Returns a semitone number for a given note, counting from C.
absSemitone :: ABC -> Integer
absSemitone = toInteger . fromEnum