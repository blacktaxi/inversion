{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
module Instrument where

import qualified Data.Map as M
import Note

-- |A guitar string with specified open string note.
data GuitarString = GuitarString AbsNote
    deriving (Eq, Read, Show)

type StringName a = (Ord a, Eq a)
type FretNumber = Integer

data Instrument a = (StringName a) => Instrument (M.Map a GuitarString) FretNumber

-- |Creates a FretNumber -> Instrument from list of (string name, zero fret note).
fromList :: (StringName a) => [(a, AbsNote)] -> FretNumber -> Instrument a
fromList strings =
    Instrument (M.fromList $ map (\(n, no) -> (n, GuitarString no)) strings)

sixStringGuitar :: [AbsNote] -> FretNumber -> Instrument String
sixStringGuitar stringNotes =
    fromList (zip stringNames stringDefs)
    where
        stringDefs = stringNotes
        stringNames = ["1-e", "2-B", "3-G", "4-D", "5-A", "6-E"]

-- |A 6-string acoustic guitar in Standard E concert tuning.
guitar = sixStringGuitar [AbsNote E 4, AbsNote B 3, AbsNote G 3,
                         AbsNote D 3, AbsNote A 2, AbsNote E 2]
                         15

-- |A concert size ukulele in standard tuning.
ukulele = Instrument (M.fromList $ zip names defs) 18
          where names = ["1-A", "2-E", "3-C", "4-g"]
                defs = map GuitarString [AbsNote A 4, AbsNote E 4, AbsNote C 4, AbsNote G 4]