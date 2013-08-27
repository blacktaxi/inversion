{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import Data.List (sortBy)
import Data.Ord (comparing)

import Search (templateChordFingerings, chordRank, fretSpan)
import TemplateParse (parseChordTemplate)
import Print (ShowFingering (..))
import qualified Instrument as I
  
data Inversion = Search
    {instrument :: String
    ,showAll :: Bool
    ,absurdChords :: Bool
    ,chord :: String
    }
    deriving (Data, Typeable, Show, Eq)

inversion = Search
    {instrument = "ukulele" &= help "Instrument to search chords for, 'guitar' or 'ukulele'"
    ,showAll = False &= help "Show all chords or only 5 best"
    ,absurdChords = False &= help "Show chords that are impossible to fret"
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
            let chordTpl = either 
                    (error . ("Error parsing chord template: " ++) . show) 
                    id $
                    parseChordTemplate chord
                unordered = templateChordFingerings chordTpl instr
            in sortBy (comparing chordRank) unordered
        fingerings' =
            if absurdChords then fingerings
            else filter ((< 6) . fretSpan) fingerings
        fingerings'' =
            if showAll then fingerings' else take 5 fingerings'
    mapM (putStrLn . showFingering instr) fingerings''