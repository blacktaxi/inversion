module Search where

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Fingering -- (Fret, StringFingering, ChordFingering)
import Instrument -- (Instrument, GuitarString, FretNumber)
import Note (AbsNote, semitoneDistance)

-- |Finds a fingering for a note on a specific string.
findFret :: AbsNote -- ^ note
         -> GuitarString -- ^ a string
         -> FretNumber -- ^ number of frets on the string
         -> Maybe Fret -- ^ Just fret number if possible, Nothing otherwise
findFret note (GuitarString openNote) frets =
    case semitoneDistance openNote note of
    x | x == 0 -> Just Open
    x | x > 0, x <= frets -> Just $ Fret x
    _ -> Nothing

-- |Finds all possible fingerings for a single note on a given instrument.
noteFingerings :: AbsNote -> Instrument a -> [StringFingering a]
noteFingerings note (Instrument strings frets) =
    catMaybes possibleFingerings
    where
        possibleFingerings = map fretOnString nameStringPairs
        fretOnString (n, s) = fmap (StringFingering n) (findFret note s frets)
        nameStringPairs = M.assocs strings

-- |Finds all possible fingerings for a chord on a given instrument.
chordFingerings :: [AbsNote] -> Instrument a -> [ChordFingering a]
chordFingerings [] _ = []
chordFingerings [n] instrument =
    map (\x -> ChordFingering [x]) $ noteFingerings n instrument
chordFingerings (n:ns) instr =
    do fstN@(StringFingering used _) <- noteFingerings n instr
       (ChordFingering restN) <- chordFingerings ns (removeString used instr)
       return $ ChordFingering (fstN:restN)
    where removeString n (Instrument ss f) = Instrument (M.delete n ss) f