{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Pattern where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList)

import Note (Note (..), ABC (..), Octave)
import Interval (Interval (..), invert)
import Chord (Chord (..))

class GenSource a b where
    generate :: a -> [b]

instance GenSource a a where
    generate x = [x]

data PatternValue a = (Enum a, Bounded a) =>
    Any |
    Exact a |
    OneOf [a]

instance (Enum a, Bounded a) => GenSource (PatternValue a) a where
    generate Any = [minBound .. maxBound]
    generate (Exact x) = generate x
    generate (OneOf xs) = concatMap generate xs

data PatternOption a =
    ExactlyOne a |
    OneOrNone a
    --Times Integer a |
    --TimesRange Integer Ingeter a

instance GenSource (PatternOption a) (Maybe a) where
    generate (ExactlyOne x) = Just <$> generate x
    generate (OneOrNone x) = Nothing : (Just <$> generate x)

data NotePattern = NotePattern (PatternValue ABC) (PatternValue Octave)

instance GenSource NotePattern Note where
    generate (NotePattern np op) = Note <$> generate np <*> generate op

data IntervalPatternValue = IntervalPatternValue
    { interval :: Interval, inversionsAllowed :: Bool }

type IntervalPattern = PatternOption IntervalPatternValue

instance GenSource (PatternOption IntervalPatternValue) Interval where
    generate p = [ i | x <- generate p :: [Maybe IntervalPatternValue], 
        i <- concatMap generate (maybeToList x) ]

instance (GenSource a b) => GenSource [a] [b] where
    generate [] = [[]]
    generate (p:ps) = [ x : y | x <- generate p, y <- generate ps ]

instance GenSource IntervalPatternValue Interval where
    generate IntervalPatternValue {..} =
        if inversionsAllowed then [interval, invert interval]
        else [interval]

data ChordPattern a b = (GenSource a Note, GenSource b [Interval]) => ChordPattern a b

instance GenSource (ChordPattern a b) Chord where
    generate (ChordPattern ns is) = Chord <$> generate ns <*> generate is