{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import System.Environment (getArgs)
import Control.Monad (mapM)
import Chord (Chord (..))
import Interval (Interval (..))
import Note (Note (..))
import Search (chordFingerings, templateChordFingerings)
import Template (NoteTemplate, ChordTemplate (..), Singleton, TemplateValue)
import Print (ShowFingering (..))
import qualified Instrument as I
  
printChord notes chordNumber instr =
           putStrLn $ showFingering instr $ chords !! chordNumber
           where chords = chordFingerings notes instr

deriving instance (Show a, Show b) => Show (ChordTemplate a b)
deriving instance (Ord a, Read a, Ord b, Read b, TemplateValue a Note, TemplateValue b [Interval]) => Read (ChordTemplate a b)

main = do  
    (mode:iname:args) <- getArgs
    let instrument = 
            case iname of
             "ukulele" -> I.ukulele
             "guitar" -> I.guitar
             _ -> error $ "unknown instrument " ++ iname
        fingerings =
            case mode of
            "template" ->
                let chordTpl = read (unwords args) :: ChordTemplate NoteTemplate (Singleton [Interval])
                in templateChordFingerings chordTpl instrument
            "-" ->
                let (n, o, ints) = read (unwords args)
                    chord = Chord (Note n o) $ map Interval ints
                in chordFingerings chord instrument
    mapM (putStrLn . (showFingering instrument)) fingerings