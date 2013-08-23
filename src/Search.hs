{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Search where

import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))

import Fingering
import Instrument
import Interval (NoteNum (..), Interval (..))
import Note (Note (..), ABC (..), Octave)
import Chord (Chord (..), toNotes)

class TemplateValue a b where
    enumerate :: a -> [b]

data TemplateOption a = Any | Exact a | OneOf [a]
    deriving (Eq, Ord, Read, Show)
data NoteTemplate = NoteTemplate (TemplateOption ABC) (TemplateOption Octave)
    deriving (Eq, Ord, Read, Show)

instance (Enum a, Bounded a) => TemplateValue (TemplateOption a) a where
    enumerate Any = [minBound .. maxBound]
    enumerate (Exact x) = [x]
    enumerate (OneOf xs) = xs

instance TemplateValue NoteTemplate Note where
    --enumerate (NoteTemplate ns os) = [ Note n o | n <- enumerate ns, o <- enumerate os ]
    enumerate (NoteTemplate ns os) = Note <$> enumerate ns <*> enumerate os

data ChordTemplate a b = (TemplateValue a Note, TemplateValue b [Interval]) => ChordTemplate a b
deriving instance (Show a, Show b) => Show (ChordTemplate a b)
deriving instance (Ord a, Read a, Ord b, Read b, TemplateValue a Note, TemplateValue b [Interval]) => Read (ChordTemplate a b)

--instance (Enum a, Bounded a) => TemplateValue [TemplateOption a] [a] where

instance TemplateValue a a where
    enumerate x = [x]

instance TemplateValue (ChordTemplate a b) Chord where
    --enumerate (ChordTemplate ns iss) = [ Chord root is | root <- enumerate ns, is <- enumerate iss]
    enumerate (ChordTemplate ns iss) = Chord <$> enumerate ns <*> enumerate iss

-- |Finds a fingering for a note on a specific string.
findFret :: Note -- ^ note
         -> GuitarString -- ^ a string
         -> FretNumber -- ^ number of frets on the string
         -> Maybe Fret -- ^ Just fret number if possible, Nothing otherwise
findFret note (GuitarString openNote) frets =
    case note .- openNote of
    (Interval x) | x == 0 -> Just Open
    (Interval x) | x > 0, x <= frets -> Just $ Fret x
    _ -> Nothing

-- |Finds all possible fingerings to play a single note on a given instrument.
noteFingerings :: Note -> Instrument a -> [StringFingering a]
noteFingerings note (Instrument strings frets) =
    catMaybes possibleFingerings
    where
        possibleFingerings = map fretOnString nameStringPairs
        fretOnString (n, s) = fmap (StringFingering n) (findFret note s frets)
        nameStringPairs = M.assocs strings

-- |Finds all possible fingerings to play a list of notes simultaneously
-- on a given instrument.
notesFingerings :: [Note] -> Instrument a -> [[StringFingering a]]
notesFingerings [] _ = []
notesFingerings [n] instrument =
    map (: []) $ noteFingerings n instrument
notesFingerings (n:ns) instr =
    do fstN@(StringFingering used _) <- noteFingerings n instr
       restN <- notesFingerings ns (removeString used instr)
       return (fstN:restN)
    where removeString n (Instrument ss f) = Instrument (M.delete n ss) f

-- |Finds all possible fingerings for a given chord.
chordFingerings :: Chord -> Instrument a -> [ChordFingering a]
chordFingerings c i = map ChordFingering $ notesFingerings (toNotes c) i

-- ChordTemplate NoteTemplate [Interval] ?
templateChordFingerings :: ChordTemplate a b -> Instrument c -> [ChordFingering c]
templateChordFingerings c i = concatMap (\c -> chordFingerings c i) (enumerate c)