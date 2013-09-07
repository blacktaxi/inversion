{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module Fingering where

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)

import Instrument (Instrument (..), GuitarString (..), StringName, FretNumber)
import Note (Note (..), Octave (..), absSemitone)

newtype Fret = Fret FretNumber
    deriving (Eq, Ord, Read, Show)

data StringFingering a = StringName a => StringFingering a Fret
deriving instance (Show a) => Show (StringFingering a)
deriving instance (Eq a) => Eq (StringFingering a)
deriving instance (Ord a, Read a) => Read (StringFingering a)

newtype ChordFingering a = ChordFingering [StringFingering a]
    deriving (Eq, Read, Show)

-- @TODO bad name
toIntegerNotes :: Instrument a -> ChordFingering a -> [Integer]
toIntegerNotes (Instrument strings _) (ChordFingering fingerings) =
    -- @TODO fix the reverse
    map fingeringToNote $ reverse $ sortBy (comparing stringOrder) fingerings
    where
        stringOrder (StringFingering n _) = n
        fingeringToNote (StringFingering sname (Fret fret)) =
            let (GuitarString (Note abc (Octave oct))) = strings M.! sname
            -- @TODO copypasta! should probably move this to Note
            in (absSemitone abc) + (oct * 12) + fret