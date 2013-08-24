{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Template where

import Control.Applicative ((<$>), (<*>))
import Note (Note (..), ABC (..), Octave)
import Interval (Interval (..))
import Chord (Chord (..))

class TemplateValue a b where
    enumerate :: a -> [b]

data TemplateOption a = Any | Exact a | OneOf [a]
    deriving (Eq, Ord, Read, Show)

data NoteTemplate = NoteTemplate (TemplateOption ABC) (TemplateOption Octave)
    deriving (Eq, Ord, Read, Show)

instance (Enum a, Bounded a) => TemplateValue (TemplateOption a) a where
    enumerate Any = [minBound .. maxBound]
    enumerate (Exact x) = [x]
    enumerate (OneOf xs) = xs

instance TemplateValue NoteTemplate Note where
    --enumerate (NoteTemplate ns os) = [ Note n o | n <- enumerate ns, o <- enumerate os ]
    enumerate (NoteTemplate ns os) = Note <$> enumerate ns <*> enumerate os

data ChordTemplate a b = (TemplateValue a Note, TemplateValue b [Interval]) => ChordTemplate a b

--instance (Enum a, Bounded a) => TemplateValue [TemplateOption a] [a] where

-- DANGER!
instance TemplateValue a a where
    enumerate x = [x]

newtype Singleton a = Singleton a
    deriving (Eq, Ord, Read, Show)

instance TemplateValue (Singleton a) a where
    enumerate (Singleton x) = [x]

instance TemplateValue (ChordTemplate a b) Chord where
    --enumerate (ChordTemplate ns iss) = [ Chord root is | root <- enumerate ns, is <- enumerate iss]
    enumerate (ChordTemplate ns iss) = Chord <$> enumerate ns <*> enumerate iss
