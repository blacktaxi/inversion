{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fingering where

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))

import Instrument (Instrument (..), InstrumentString (..), FretNumber)
import Note (Note (..), Octave (..), absSemitone)

newtype Fret = Fret FretNumber
    deriving (Eq, Ord, Read, Show, Num)

data StringFingering = StringFingering String (Maybe Fret)
    deriving (Show, Eq, Ord, Read)

newtype ChordFingering = ChordFingering [StringFingering]
    deriving (Read, Show)

normalizeFingering :: ChordFingering -> ChordFingering
normalizeFingering (ChordFingering fingerings) =
    -- @TODO fix the reverse
    ChordFingering . reverse $ sortBy (comparing stringOrder) fingerings
    where
        stringOrder (StringFingering n _) = n

instance Eq ChordFingering where
    f1 == f2 =
        norm f1 == norm f2
        where
            stringOrder (StringFingering n _) = n
            norm (ChordFingering fingerings) =
                sortBy (comparing stringOrder) fingerings

-- @TODO bad name
toIntegerNotes :: Instrument -> ChordFingering -> [Integer]
toIntegerNotes (Instrument strings _) f =
    catMaybes $ map fingeringToNote fingerings
    where
        stringsMap = M.fromList strings
        (ChordFingering fingerings) = normalizeFingering f

        fingeringToNote (StringFingering sname maybeFret) =
            fretToNote <$> maybeFret
            where
                fretToNote (Fret fret) =
                    let (InstrumentString (Note abc (Octave oct))) = stringsMap M.! sname
                    -- @TODO copypasta! should probably move this to Note
                    in (absSemitone abc) + (oct * 12) + fret