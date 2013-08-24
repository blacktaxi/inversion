{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Print
       (ShowFingering (..)) where

import Data.List (delete, genericReplicate, sortBy)
import qualified Data.Map as M
import Text.Printf (printf, PrintfArg)

import Instrument
import Fingering

class ShowFingering b a where
    showFingering :: Instrument b -> a -> String

showFingeredString :: (PrintfArg a) => GuitarString -> a -> FretNumber -> Maybe Fret -> String
showFingeredString string stringName fretCount fingering =
    printf "%5s %s%s" stringName nut fretBoard
    where 
        emptyBoard = concat $ genericReplicate fretCount "---|"
        (nut, fretBoard) =
            case fingering of
            Nothing -> ("x", emptyBoard)
            Just Open -> ("O", emptyBoard)
            Just (Fret fret) -> ("|", concat [if f == fret then "-O-|" else "---|" |
                                             f <- [1 .. fretCount]])

showFretNumbers fretCount =
    concat $ map (printf "%4d") [1 .. fretCount]

showNumberedBoard board fretCount =
    unlines (board ++ [fretNumbers])
    where fretNumbers = "     " ++ (showFretNumbers fretCount)
          
instance (PrintfArg a) => ShowFingering a (StringFingering a) where
    showFingering (Instrument strings fretCount) (StringFingering name fingering) =
        showNumberedBoard ss fretCount
        where
            mutedString s n = showFingeredString s n fretCount Nothing
            ss = [if n == name then showFingeredString s n fretCount (Just fingering)
                 else mutedString s n | (n, s) <- M.assocs strings]

instance (PrintfArg a) => ShowFingering a (ChordFingering a) where
    showFingering (Instrument strings fretCount) (ChordFingering fingerings) =
        showNumberedBoard ss fretCount
        where
            allMute = M.map (\_ -> Nothing :: Maybe (StringFingering a)) strings
            chord = M.fromList $ map (\f@(StringFingering n _) -> (n, Just f)) fingerings
            stringMap = chord `M.union` allMute
            ss = [showFingeredString string n fretCount maybeFret |
                 (n, f) <- M.assocs stringMap,
                 let string = strings M.! n,
                 let maybeFret = fmap (\(StringFingering _ fret) -> fret) f]