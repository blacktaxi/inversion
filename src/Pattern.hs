{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Pattern where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList, catMaybes)

import Note (Note (..), ABC (..), Octave)
import Interval (Interval (..))
import qualified Interval as In
import Chord (Chord (..))

class GenSource a b where
    generate :: a -> [b]

instance GenSource a a where
    generate x = [x]

data PatternValue a = (Enum a, Bounded a) =>
    Any |
    Exact a |
    OneOf [a]
deriving instance (Show a) => Show (PatternValue a)

instance (GenSource a a, Enum a, Bounded a) => GenSource (PatternValue a) a where
    generate Any = [minBound .. maxBound]
    generate (Exact x) = generate x
    generate (OneOf xs) = concatMap generate xs

-- |An interval in a chord spec.
-- @TODO rename fields
data IntervalPattern = IntervalPattern
    { interval :: Interval
    , isOptional :: Bool
    , canUseMany :: Bool
    , fixedOctave :: Bool
    }
    deriving (Eq, Show, Read)

data NotePattern = NotePattern (PatternValue ABC) (PatternValue Octave)
    deriving (Show)

instance GenSource NotePattern Note where
    generate (NotePattern np op) = Note <$> generate np <*> generate op

-- |
data ChordPattern = ChordPattern NotePattern [IntervalPattern]
    deriving (Show)

--data PatternOption a =
--    ExactlyOne a |
--    OneOrNone a


--data IntervalPatternValue = IntervalPatternValue
--    { interval :: Interval, inversionsAllowed :: Bool }

--type IntervalPattern = PatternOption IntervalPatternValue

--data ChordPattern a b = (GenSource a Note, GenSource b [Interval]) =>
--    ChordPattern a b

--instance (GenSource a a) => GenSource (PatternOption a) (Maybe a) where
--    generate (ExactlyOne x) = Just <$> generate x
--    generate (OneOrNone x) = Nothing : (Just <$> generate x)

--instance GenSource [PatternOption a] [a] where
--    generate p = map catMaybes $ sequence $ map generate p

--instance GenSource [IntervalPattern] [Interval] where
--    generate ps = concatMap (sequence . map generate) (generate ps :: [[IntervalPatternValue]])


--instance GenSource IntervalPatternValue Interval where
--    generate IntervalPatternValue {..} =
--        if inversionsAllowed then
--            -- @TODO incomplete
--            [interval, interval + In.octave, interval - In.octave]
--        else [interval]

--instance GenSource (ChordPattern a b) Chord where
--    generate (ChordPattern ns is) = Chord <$> generate ns <*> generate is