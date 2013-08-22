{-# LANGUAGE PatternGuards #-}
module Search where

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Fingering
import Instrument
import Interval (NoteNum (..), Interval (..))
import Note (Note)
import Chord (Chord(..), toNotes)

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

-- |Finds all possible fingerings to play a list of notes on a given instrument.
-- @TODO [ChordFingering a] isn't really appropriate.
notesFingerings :: [Note] -> Instrument a -> [ChordFingering a]
notesFingerings [] _ = []
notesFingerings [n] instrument =
    map (\x -> ChordFingering [x]) $ noteFingerings n instrument
notesFingerings (n:ns) instr =
    do fstN@(StringFingering used _) <- noteFingerings n instr
       (ChordFingering restN) <- notesFingerings ns (removeString used instr)
       return $ ChordFingering (fstN:restN)
    where removeString n (Instrument ss f) = Instrument (M.delete n ss) f

chordFingerings :: Chord -> Instrument a -> [ChordFingering a]
chordFingerings = notesFingerings . toNotes