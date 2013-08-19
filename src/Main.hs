module Main where

import System.Environment (getArgs)
import Control.Monad (mapM)
import Search (chordFingerings)
import Print (ShowFingering (..))
import qualified Instrument as I
  
printChord notes chordNumber instr =
           putStrLn $ showFingering instr $ chords !! chordNumber
           where chords = chordFingerings notes instr

main = do  
    (iname:chordSpec) <- getArgs
    let chord = read (unwords chordSpec)
        instrument = case iname of
                     "ukulele" -> I.ukulele
                     "guitar" -> I.guitar
                     _ -> error $ "unknown instrument " ++ iname
        fingerings = chordFingerings chord instrument
    mapM (putStrLn . (showFingering instrument)) fingerings