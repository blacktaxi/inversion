module Main where

import System.Environment (getArgs)
import Control.Monad (mapM)
import Chord (Chord (..))
import Interval (Interval (..))
import Note (Note (..))
import Search (chordFingerings, templateChordFingerings, NoteTemplate (..), ChordTemplate (..))
import Print (ShowFingering (..))
import qualified Instrument as I
  
printChord notes chordNumber instr =
           putStrLn $ showFingering instr $ chords !! chordNumber
           where chords = chordFingerings notes instr

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
                let chordTpl = read (unwords args) :: ChordTemplate NoteTemplate [Interval]
                in templateChordFingerings chordTpl instrument
            "-" ->
                let (n, o, ints) = read (unwords args)
                    chord = Chord (Note n o) $ map Interval ints
                in chordFingerings chord instrument
    mapM (putStrLn . (showFingering instrument)) fingerings