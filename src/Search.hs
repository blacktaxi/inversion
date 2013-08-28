{-# LANGUAGE PatternGuards #-}
module Search where

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Fingering
import Instrument
import Interval (NoteNum (..), Interval (..))
import Note (Note (..))
import Chord (Chord (..), toNotes)
import Pattern (GenSource (..), ChordPattern)

-- |Finds a fingering for a note on a specific string.
findFret :: Note -- ^ note
         -> GuitarString -- ^ a string
         -> FretNumber -- ^ number of frets on the string
         -> Maybe Fret -- ^ Just fret number if possible, Nothing otherwise
findFret note (GuitarString openNote) frets =
    case note .- openNote of
    (Interval x) | x == 0 -> Just (Fret 0)
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
    where removeString name (Instrument ss f) = Instrument (M.delete name ss) f

-- |Finds all possible fingerings for a given chord.
chordFingerings :: Chord -> Instrument a -> [ChordFingering a]
chordFingerings c i = map ChordFingering $ notesFingerings (toNotes c) i

-- ChordTemplate NoteTemplate [Interval] ?
templateChordFingerings :: ChordPattern a b -> Instrument c -> [ChordFingering c]
templateChordFingerings c i = concatMap (`chordFingerings` i) (generate c)

fretSpan :: ChordFingering a -> Integer
fretSpan (ChordFingering []) = 0
fretSpan (ChordFingering ss) =
    case fingered of
    [] -> 0
    fs -> maximum fs - minimum fs
    where
        fretNums = map (\(StringFingering _ (Fret x)) -> x) ss
        fingered = filter (/= 0) fretNums

--fretSpan (ChordFingering ss) =
--    sum $ map (\x -> abs $ if x /= 0 then origin - x else 0) fretNums
--    where
--        fretNums = map (\(StringFingering _ (Fret x)) -> x) ss
--        origin = 
--            case filter (/= 0) fretNums of
--            [] -> 0
--            fs -> minimum fs

frettable :: ChordFingering a -> Bool
frettable f = fretSpan f < 6

chordRank (ChordFingering []) = error "fingering for 0 strings?"
chordRank c@(ChordFingering ss) =
    (-usedStrings, -openStrings, fretSpan c)
    where
        usedStrings = length ss
        openStrings = length $ filter (\(StringFingering _ (Fret x)) -> x == 0) ss