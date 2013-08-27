module TemplateParse (parseChordTemplate) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (*>), (<*), (<$))
import Data.Maybe (fromMaybe, isJust)

import Note (ABC (..), Octave (..))
import Interval (Interval (..))
import qualified Interval as In
import Pattern

-- <chord> :: <root_note>["("<octave_range>")"](<explicit_intervals>|<chord_spec>)
-- <octave_range> :: \d|(\d,\d,...)|(\d-\d)
-- <explicit_intervals> :: "{" (<interval_spec> ",")* "}"
-- <interval_spec> :: 
--      {0=,4,7?,11=?} -- 0 exact, 4 with inversions allowed, optional 7 with inversions, optional 11 exact
--      {0=,4,*}-{10,11} -- 0, 4 and any other, except 10 and 11
--      {0=,4,7?,10|11} -- ..., 10 or 11
--      {}
-- <chord_spec> :: <chord_quality><interval_num><altered_fifth><additional_interval>
-- <chord_quality> :: "m" | "M" | ""
-- <interval_num> :: "(" ("maj" | "min" | "aug" | "dim")<number> ")"

infix 0 <??>
(<??>) = flip (<?>)

oneOfStr :: [String] -> CharParser () String
oneOfStr ss = choice (map string ss)

pChord :: CharParser () (ChordPattern NotePattern [IntervalPattern])
pChord = do
    note <- "note pattern" <??> templateValue pNote
    octave <- "octave pattern" <??> fromMaybe Any <$> 
        optionMaybe (char '(' *> templateValue pOctave <* char ')')
    --intervals' <- specIntervals <|> explicitIntervals
    intervalPatterns <- "chord spec" <??> pExplicitIntervals
    return $ ChordPattern (NotePattern note octave) intervalPatterns

pNote :: CharParser () ABC
pNote = do
    n <- oneOf "ABCDEFG"
    m <- maybe [] ((: []) . sharpOrFlat) <$> optionMaybe (oneOf "#b")
    return $ read (n : m)
    where 
        sharpOrFlat '#' = 's'
        sharpOrFlat x = x

pOctave :: CharParser () Octave
pOctave = Octave . read . (: []) <$> oneOf "012345678"

pIntervalName :: CharParser () Interval
pIntervalName =  "interval name" <??> choiceInterval
    [(In.perf1, ["P1", "perf1", "dim2"])
    ,(In.min2, ["m2", "min2", "A1", "aug1"])
    ,(In.maj2, ["M2", "maj2", "d3", "dim3"])
    ,(In.min3, ["m3", "min3", "A2", "aug2"])
    ,(In.maj3, ["M3", "maj3", "d4", "dim4"])
    ,(In.perf4, ["P4", "perf4", "A3", "aug3"])
    ,(In.dim5, ["d5", "dim5", "A4", "aug4", "tritone"])
    ,(In.perf5, ["P5", "perf5", "d6", "dim6"])
    ,(In.min6, ["m6", "min6", "A5", "aug5"])
    ,(In.maj6, ["M6", "maj6", "d7", "dim7"])
    ,(In.min7, ["m7", "min7", "A6", "aug6"])
    ,(In.maj7, ["M7", "maj7", "d8", "dim8"])
    ,(In.perf8, ["P8", "perf8", "A7", "aug7"])]
    where
        p_oneName (interval, names) = try $ interval <$ oneOfStr names
        choiceInterval is = choice (map p_oneName is)

pIntervalInteger :: CharParser () Interval
pIntervalInteger = (Interval . read) <$> many1 digit

pInterval :: CharParser () Interval
pInterval = pIntervalInteger <|> pIntervalName

pIntervalPattern :: CharParser () IntervalPattern
pIntervalPattern = do
    noInversions <- isJust <$> optionMaybe (char '=')
    intervalDef <- pInterval
    isOptional <- isJust <$> optionMaybe (char '?')
    let ipv = IntervalPatternValue { interval = intervalDef, inversionsAllowed = not noInversions }
    let po = if isOptional then OneOrNone ipv else ExactlyOne ipv
    return $ po

pExplicitIntervals :: CharParser () [IntervalPattern]
pExplicitIntervals = char '{' *> sepBy1 pIntervalPattern (char ',') <* char '}'

templateValue :: (Bounded a, Enum a) => CharParser () a -> CharParser () (PatternValue a)
templateValue p = maybe Any OneOf <$> optionMaybe oneOrManyV
    where 
        oneV = (: []) <$> p
        manyV = "pattern" <??> char '[' *> sepBy1 p (char ',') <* char ']'
        oneOrManyV = manyV <|> oneV

parseChordTemplate :: String 
    -> Either ParseError (ChordPattern NotePattern [IntervalPattern])
parseChordTemplate = parse pChord "(chord def)"
