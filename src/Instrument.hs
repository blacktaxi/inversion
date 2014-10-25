{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
module Instrument where

import Control.Arrow as A
import qualified Data.Map as M

import Note


--class InstrumentData i where
--    create :: a -> i

--data InstrumentData i a = InstrumentData a

--data GuitarData a = (a, a, a, a, a, a)
--data UkuleleData a = (a, a, a, a)

-- |A guitar string with specified open string note.
newtype InstrumentString = InstrumentString Note
    deriving (Eq, Read, Show)

--type StringName a = (Ord a, Eq a)
type FretNumber = Integer

data Instrument = Instrument [(String, InstrumentString)] FretNumber

-- |Creates a FretNumber -> Instrument from list of (string name, zero fret note).
--fromList :: (StringName a) => [(a, Note)] -> FretNumber -> Instrument a
--fromList strings =
--    Instrument (M.fromList $ map (A.second GuitarString) strings)
fromList names notes =
    Instrument $ zip names (map InstrumentString notes)

sixStringGuitar :: [Note] -> FretNumber -> Instrument
sixStringGuitar stringNotes =
    fromList stringNames stringNotes
    where
        stringNames = ["1-e", "2-B", "3-G", "4-D", "5-A", "6-E"]

-- |A 6-string electric guitar in Standard E concert tuning with 21 fret.
guitar = sixStringGuitar [Note E 5, Note B 4, Note G 4,
                         Note D 4, Note A 3, Note E 3]
                         21

-- |A concert size ukulele in standard tuning.
ukulele = fromList names defs 18
          where names = ["1-A", "2-E", "3-C", "4-g"]
                defs = [Note A 5, Note E 5, Note C 5, Note G 5]