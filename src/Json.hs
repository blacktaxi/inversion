{-# LANGUAGE FlexibleInstances #-}
module Json () where

import qualified Data.Map as M
import Control.Applicative ((<$>))

import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Instrument
import Fingering
import Note

jsonObj :: [(String, JSValue)] -> JSValue
jsonObj defs =
    JSObject $ Text.JSON.Types.JSONObject { fromJSObject = defs }

toJSDefs :: (JSON a) => [(String, a)] -> [(String, JSValue)]
toJSDefs = map (\(k, v) -> (k, showJSON v))

newtype MapJSObject k v = MapJSObject { getMap :: M.Map k v }

instance (JSON v) => JSON (MapJSObject String v) where
    readJSON = undefined
    showJSON (MapJSObject { getMap = m }) =
        jsonObj . toJSDefs $ M.toList m

instance JSON ABC where
    readJSON = undefined
    showJSON x = showJSON $ show x

instance JSON Octave where
    readJSON = undefined
    showJSON (Octave o) = showJSON o

instance JSON Note where
    readJSON = undefined
    showJSON (Note abc octave) = showJSON (abc, octave)

instance JSON InstrumentString where
    readJSON = undefined
    showJSON (InstrumentString n) = showJSON n

instance JSON Instrument where
    readJSON = undefined
    showJSON (Instrument strings frets) =
        jsonObj [
            ("frets", showJSON frets),
            ("strings", showJSON $ MapJSObject { getMap = M.fromList strings })
        ]

instance JSON ChordFingering where
    readJSON = undefined
    showJSON (ChordFingering fingerings) =
        jsonObj . toJSDefs $ 
            map (\(StringFingering name fret) -> (name, (\(Fret f) -> f) <$> fret)) $
            fingerings