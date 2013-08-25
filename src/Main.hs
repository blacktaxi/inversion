{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import Chord ()
import Interval (Interval (..))
import Note (Note (..))
import Search (templateChordFingerings)
import Template (NoteTemplate, ChordTemplate (..), Singleton, TemplateValue)
import TemplateParse (parseChordTemplate)
import Print (ShowFingering (..))
import qualified Instrument as I
  
data Inversion = Search
    {instrument :: String
    ,chord :: String
    }
    deriving (Data, Typeable, Show, Eq)

inversion = Search
    {instrument = def &= opt "guitar" &= help "Instrument to search chords for, 'guitar' or 'ukulele'"
    ,chord = def &= typ "CHORD DEF" &= help "A chord definition"
    } &=
    program "inversion" &=
    help "Search for chord fingerings" &=
    summary "Inversion v0.0.1.0, (c) Sergey Yavnyi"

main = do
    Search{..} <- cmdArgs inversion
    let instr = 
            case instrument of
             "ukulele" -> I.ukulele
             "guitar" -> I.guitar
             x -> error $ "unknown instrument " ++ x
        fingerings =
            let chordTpl = either (error . show) id $ parseChordTemplate "CHORD DEF" chord
            in templateChordFingerings chordTpl instr
    mapM (putStrLn . showFingering instr) fingerings