{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
module Instrument where

import Control.Arrow as A
import qualified Data.Map as M

import Note

-- |A guitar string with specified open string note.
newtype InstrumentString a = InstrumentString Note
    deriving (Eq, Read, Show)

type StringName a = (Ord a, Eq a)
type FretNumber = Integer

data Instrument a = (StringName a) => Instrument (M.Map a GuitarString) FretNumber

-- |Creates a FretNumber -> Instrument from list of (string name, zero fret note).
fromList :: (StringName a) => [(a, Note)] -> FretNumber -> Instrument a
fromList strings =
    Instrument (M.fromList $ map (A.second GuitarString) strings)

sixStringGuitar :: [Note] -> FretNumber -> Instrument String
sixStringGuitar stringNotes =
    fromList (zip stringNames stringDefs)
    where
        stringDefs = stringNotes
        stringNames = ["1-e", "2-B", "3-G", "4-D", "5-A", "6-E"]

-- |A 6-string electric guitar in Standard E concert tuning with 21 fret.
guitar = sixStringGuitar [Note E 5, Note B 4, Note G 4,
                         Note D 4, Note A 3, Note E 3]
                         21

-- |A concert size ukulele in standard tuning.
ukulele = fromList (zip names defs) 18
          where names = ["1-A", "2-E", "3-C", "4-g"]
                defs = [Note A 5, Note E 5, Note C 5, Note G 5]