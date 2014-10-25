{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Search where

import Data.List (nub, deleteBy)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Monad (guard)
import Control.Applicative ((<$>))

import Fingering
import Instrument
import Interval (Interval (..), intervalBetween, addInterval, multiplyInterval)
import qualified Interval as In
import Note (Note (..), Octave (..))
import Chord (Chord (..), toNotes)
import Pattern (GenSource (..), ChordPattern (..), NotePattern (..),
    IntervalPattern (..))

-- |Finds a fingering for a note on a specific string.
findFret :: Note -- ^ note
         -> InstrumentString -- ^ a string
         -> Fret
findFret note (InstrumentString openNote) =
    Fret i
    where (Interval i) = intervalBetween note openNote

-- |Finds all possible fingerings to play a single note on a given instrument.
noteFingerings :: Note -> Instrument -> [StringFingering]
noteFingerings note (Instrument strings frets) =
    possibleFingerings
    where
        possibleFingerings = map fretOnString nameStringPairs
        fretOnString (n, s) = StringFingering n (Just $ findFret note s)
        nameStringPairs = strings

-- |Finds all possible fingerings to play a list of notes simultaneously
-- on a given instrument.
--notesFingerings :: [Note] -> Instrument -> [[StringFingering]]
--notesFingerings [] _ = []
--notesFingerings [n] instrument =
--    map (: []) $ noteFingerings n instrument
--notesFingerings (n:ns) instr =
--    do fstN@(StringFingering used _) <- noteFingerings n instr
--       restN <- notesFingerings ns (removeString used instr)
--       return (fstN:restN)
--    where
--        removeString name (Instrument ss f) =
--            Instrument (deleteBy (\(n1, _) (n2, _) -> n1 == n2) name ss) f

-- |Finds all possible fingerings for a given chord.
chordFingerings :: ChordPattern -> Instrument -> [ChordFingering]
--chordFingerings c i = map ChordFingering $ notesFingerings (toNotes c) i
chordFingerings (ChordPattern root intervals) (Instrument strings frets) =
    map makeChordFingering fs
    where
        makeChordFingering frets =
            ChordFingering $
            map (uncurry StringFingering) $ zip stringNames frets
        stringNames = map fst strings
        stringDefs = map snd strings
        fs = fingerings intervals root stringDefs frets

-- ChordTemplate NoteTemplate [Interval] ?
--templateChordFingerings :: ChordPattern a b -> Instrument -> [ChordFingering]
--templateChordFingerings c i = nub $ concatMap (`chordFingerings` i) (generate c)

-- |Calculate how 'wide' the fingering is.
fretSpan :: ChordFingering -> Integer
fretSpan (ChordFingering []) = 0
fretSpan (ChordFingering ss) =
    case fingered of
    [] -> 0
    fs -> maximum fs - minimum fs
    where
        fretNums = map (\(Fret x) -> x) $ catMaybes $ map (\(StringFingering _ f) -> f) ss
        fingered = filter (/= 0) fretNums

--fretSpan (ChordFingering ss) =
--    sum $ map (\x -> abs $ if x /= 0 then origin - x else 0) fretNums
--    where
--        fretNums = map (\(StringFingering _ (Fret x)) -> x) ss
--        origin = 
--            case filter (/= 0) fretNums of
--            [] -> 0
--            fs -> minimum fs

frettable :: ChordFingering -> Bool
frettable f = fretSpan f < 6

chordRank (ChordFingering []) = error "fingering for 0 strings?"
chordRank c @ (ChordFingering ss) =
    (-usedStrings, -openStrings, fretSpan c)
    where
        frets = map (\(StringFingering _ f) -> f) ss
        usedStrings = length $ catMaybes frets
        openStrings =
            length $ filter (\(Fret x) -> x == 0) (catMaybes frets)

-- |Finds all notes which will constitute a specified interval
-- with a specified root note.
notesForChordInterval :: Interval -> Bool -> Note -> [Note]
notesForChordInterval interval fixedOctave root =
    case fixedOctave of
        True -> [root `addInterval` interval]
        False -> 
            [root `addInterval` i | i <- intervals, (abs i) >= interval]
            where
                -- @TODO negative octaves. will this work?
                octaves =
                    map (\(Octave o) -> In.octave `multiplyInterval` o) $
                    [minBound .. maxBound] >>= (\o -> [o, -o])
                intervals = map (interval +) octaves

-- |Generate all possible selections of an interval from available interval
-- list.
pickIntervals :: [IntervalPattern] -> [(IntervalPattern, [IntervalPattern])]
pickIntervals availableIntervals = do
    (i @ IntervalPattern {..}, restIntervals) <- choice availableIntervals
    if canUseMany then return (i, availableIntervals)
    else return (i, restIntervals)
    where
        choice' _ [] acc = acc
        choice' ls (r:rs) acc =
            choice' (r:ls) rs $ (r, ls ++ rs) : acc
        choice xs = choice' [] xs []

-- |Find all possible fingerings of any of the given available chord spec
-- intervals on a given string.
intervalsOnString :: [IntervalPattern]
                  -> Note
                  -> InstrumentString
                  -> [(Fret, [IntervalPattern])]
intervalsOnString intervals rootNote string = do
    (IntervalPattern {..}, restIntervals) <-
        pickIntervals intervals
    note <- notesForChordInterval interval fixedOctave rootNote
    let fret = findFret note string
    return $ (fret, restIntervals)

-- |Find all fingerings for the chord spec on a given instrument spec.
fingerings :: [IntervalPattern] -- ^ chord spec
           -> NotePattern -- ^ chord root note
           -> [InstrumentString] -- ^ instrument strings
           -> FretNumber -- ^ how many frets the instrument has
           -> [[Maybe Fret]] -- ^ fingerings
fingerings chordIntervals _ [] _ = do
    -- if we have any non-optional intervals left to assign but no available
    -- strings, this fingering combination should be discarded
    --guard $ any (not . isOptional) chordIntervals
    return []
fingerings chordIntervals notePattern strings frets = do
    rootNote <- generate notePattern
    -- take next string, keeping what's left
    let (string:restStrings) = strings
    -- take next fingering for any of the intervals on this string
    (fret, restIntervals) <-
        (map (\(x, z) -> ((Just x), z)) $
            intervalsOnString chordIntervals rootNote string)
        ++ [(Nothing, chordIntervals)]
    -- the fingering should be within the instrument's fretboard
    guard $ case fret of
        Nothing -> True
        Just fret -> fret >= 0 && fret <= (fromInteger frets)
    --guard $ maybe True (\fret -> fret >= 0 && fret <= (fromInteger frets)) fret

    -- take next fingerings for the rest of the strings (and intervals)
    -- @TODO filter out absurd chords here
    (fret :) <$> fingerings restIntervals notePattern restStrings frets
    --f <- fingerings restIntervals notePattern restStrings frets
    --return $ fret : f