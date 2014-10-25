{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Search where

import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Monad (guard)

import Fingering
import Instrument
import Interval (Interval (..), intervalBetween, addInterval, multiplyInterval)
import qualified Interval as In
import Note (Note (..), Octave (..))
import Chord (Chord (..), toNotes)
import Pattern (GenSource (..), ChordPattern)

-- |Finds a fingering for a note on a specific string.
findFret :: Note -- ^ note
         -> GuitarString -- ^ a string
         -> Fret
findFret note (GuitarString openNote) =
    Fret i
    where (Interval i) = intervalBetween note openNote

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
templateChordFingerings :: (Ord c, Eq c) => ChordPattern a b -> Instrument c -> [ChordFingering c]
templateChordFingerings c i = nub $ concatMap (`chordFingerings` i) (generate c)

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



---------------


--class InstrumentData i where
--    create :: a -> i

--data InstrumentData i a = InstrumentData a

--data GuitarData a = (a, a, a, a, a, a)
--data UkuleleData a = (a, a, a, a)


-- |An interval in a chord spec.
data ChordInterval = ChordInterval
    { getInterval :: Interval
    , getIsOptional :: Bool
    , getCanUseMany :: Bool
    , getFlexibleOctave :: Bool
    }

-- |Finds all notes which will constitute a specified interval
-- with a specified root note.
notesForChordInterval :: ChordInterval -> Note -> [Note]
notesForChordInterval (ChordInterval {..}) root =
    case getFlexibleOctave of
        False -> [root `addInterval` getInterval]
        True -> 
            [root `addInterval` i | i <- intervals, (abs i) >= getInterval]
            where
                -- @TODO negative octaves. will this work?
                octaves =
                    map (\(Octave o) -> In.octave `multiplyInterval` o) $
                    [(minBound :: Octave) ..] >>= (\o -> [o, -o])
                intervals = map (getInterval +) octaves

-- |Generate all possible selections of an interval from available interval
-- list.
pickIntervals :: [ChordInterval] -> [(ChordInterval, [ChordInterval])]
pickIntervals availableIntervals = do
    (i @ ChordInterval {..}, restIntervals) <- choice availableIntervals
    if getCanUseMany then return (i, availableIntervals)
    else return (i, restIntervals)
    where
        choice' _ [] acc = acc
        choice' ls (r:rs) acc =
            choice' (r:ls) rs $ (r, ls ++ rs) : acc
        choice xs = choice' [] xs []

-- |Find all possible fingerings of any of the given available chord spec
-- intervals on a given string.
intervalsOnString :: [ChordInterval]
                  -> Note
                  -> GuitarString
                  -> [(Fret, ChordInterval, [ChordInterval])]
intervalsOnString intervals rootNote string = do
    (interval, restIntervals) <- pickIntervals intervals
    note <- notesForChordInterval interval rootNote
    let fret = findFret note string
    return $ (fret, interval, restIntervals)

fingerings :: [ChordInterval]   -- ^ chord spec
           -> Note              -- ^ chord root note
           -> [GuitarString]    -- ^ instrument strings
           -> FretNumber        -- ^ how many frets the instrument has
           -> [[Fret]]          -- ^ fingerings
fingerings chordIntervals _ [] _ = do
    -- if we have any non-optional intervals left to assign but no available
    -- strings, this fingering combination should be discarded
    guard $ any (not . getIsOptional) chordIntervals
    return []
fingerings chordIntervals rootNote strings frets = do
    -- take next string, keeping what's left
    (string, restStrings) <- zip strings (tail (iterate tail strings))
    -- take next fingering for any of the intervals on this string
    (fret, _, restIntervals) <- intervalsOnString chordIntervals rootNote string
    -- the fingering should be withing the instruments fretboard
    guard $ fret >= 0 && fret <= (fromInteger frets)
    -- take next fingerings for the rest of the strings (and intervals)
    f <- fingerings restIntervals rootNote restStrings frets
    return $ fret : f