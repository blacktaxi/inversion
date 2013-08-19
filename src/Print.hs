{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Print where

import Data.List (delete, genericReplicate, sortBy)
import qualified Data.Map as M
import Text.Printf (printf, PrintfArg)

import Note
import Instrument
import Fingering
import Search

class ShowFingering b a where
    showFingering :: Instrument b -> a -> String

showFingeredString :: (PrintfArg a) => GuitarString -> a -> FretNumber -> Maybe Fret -> String
showFingeredString string stringName fretCount Nothing =
    printf "%4s x%s" stringName frets
    where frets = concat $ genericReplicate fretCount "---|"
showFingeredString string stringName fretCount (Just Open) =
    printf "%4s O%s" stringName frets
    where frets = concat $ genericReplicate fretCount "---|"
showFingeredString string stringName fretCount (Just (Fret fret)) =
    printf "%4s |%s" stringName frets
    where frets = concat [if f == fret then "-O-|" else "---|" |
                         f <- [1 .. fretCount]]

instance (PrintfArg a) => ShowFingering a (StringFingering a) where
    showFingering (Instrument strings fretCount) (StringFingering name fingering) =
        unlines ss
        where
            mutedString s n = showFingeredString s n fretCount Nothing
            ss = [if n == name then showFingeredString s n fretCount (Just fingering)
                 else mutedString s n | (n, s) <- M.assocs strings]

instance (PrintfArg a) => ShowFingering a (ChordFingering a) where
    showFingering (Instrument strings fretCount) (ChordFingering fingerings) =
        unlines ss
        where
            allMute = M.map (\_ -> Nothing :: Maybe (StringFingering a)) strings
            chord = M.fromList $ map (\f@(StringFingering n _) -> (n, Just f)) fingerings
            stringMap = chord `M.union` allMute
            ss = [showFingeredString string n fretCount maybeFret |
                 (n, f) <- M.assocs stringMap,
                 let string = strings M.! n,
                 let maybeFret = fmap (\(StringFingering _ f) -> f) f]

printChord notes chordNumber instr =
           putStrLn $ showFingering instr $ chords !! chordNumber
           where chords = chordFingerings notes instr
