#!/usr/bin/env stack
-- stack --resolver lts-3.7 --install-ghc runghc
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import System.Console.CmdArgs
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M

import Text.JSON
import Text.JSON.Types

import Search (templateChordFingerings, chordRank, fretSpan, frettable)
import TemplateParse (parseChordTemplate)
import Print (ShowFingering (..))
import Fingering (toIntegerNotes)
import qualified Instrument as I
import qualified Json as J

data InversionOutputMode = Pretty | Json | Midi
    deriving (Data, Typeable, Show, Eq)
  
data Inversion = Search
    {instrument :: String
    ,showAll :: Bool
    ,absurdChords :: Bool
    ,outputMode :: InversionOutputMode
    ,chord :: String
    }
    deriving (Data, Typeable, Show, Eq)

inversion = Search
    {instrument = "ukulele" &= help "Instrument to search chords for, 'guitar' or 'ukulele'"
    ,showAll = False &= help "Show all chords or only 5 best"
    ,absurdChords = False &= help "Show chords that are impossible to fret"
    ,outputMode = Pretty &= help "Select output mode"
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
            else filter frettable fingerings
        fingerings'' =
            if showAll then fingerings' else take 5 fingerings'

    case outputMode of
        Pretty -> mapM (putStrLn . showFingering instr) fingerings''
        Json ->
            let x = [("instrument", showJSON instr)
                    ,("chords", showJSON fingerings'')]
                output = JSObject $ Text.JSON.Types.JSONObject { fromJSObject = x }
            in
                mapM (putStrLn . encode) [output]
        Midi ->
            mapM (putStrLn . show . toIntegerNotes instr) fingerings''