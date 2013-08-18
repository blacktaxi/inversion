{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module Fingering where

import Instrument (StringName, FretNumber)

data Fret = Open | Fret FretNumber
    deriving (Eq, Ord, Read, Show)

data StringFingering a = StringName a => StringFingering a Fret
deriving instance (Show a) => Show (StringFingering a)
deriving instance (Eq a) => Eq (StringFingering a)
deriving instance (Ord a, Read a) => Read (StringFingering a)

data ChordFingering a = ChordFingering [StringFingering a]
    deriving (Eq, Read, Show)