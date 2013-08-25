module TemplateParse (parseChordTemplate) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (*>), (<*), (<$))
import Data.Maybe (fromMaybe)

import Note (ABC (..), Octave (..))
import Interval (Interval (..))
import qualified Interval as In
import Template

-- <chord> :: <root_note>["("<octave_range>")"](<explicit_intervals>|<chord_spec>)
-- <octave_range> :: \d|(\d,\d,...)|(\d-\d)
-- <explicit_intervals> :: "{"\d,\d,\d,..."}"
-- <chord_spec> :: <chord_quality><interval_num><altered_fifth><additional_interval>
-- <chord_quality> :: "m" | "M" | ""
-- <interval_num> :: "(" ("maj" | "min" | "aug" | "dim")<number> ")"

pChord :: CharParser () (ChordTemplate NoteTemplate [Interval])
pChord = do
    note <- templateValue pNote
    octave <- fromMaybe Any <$> 
        optionMaybe (char '(' *> templateValue pOctave <* char ')')
    --intervals' <- specIntervals <|> explicitIntervals
    intervals <- pExplicitIntervals
    return $ ChordTemplate (NoteTemplate note octave) intervals

pNote :: CharParser () ABC
pNote = do
    n <- oneOf "ABCDEFG"
    m <- maybe [] ((: []) . sharpOrFlat) <$> optionMaybe (oneOf "#b")
    return $ read ([n] ++ m)
    where 
        sharpOrFlat '#' = 's'
        sharpOrFlat x = x

pOctave :: CharParser () Octave
pOctave = Octave . read . (: []) <$> oneOf "012345678"

oneOfStr :: [String] -> CharParser () String
oneOfStr ss = choice (map string ss)

pIntervalName :: CharParser () Interval
pIntervalName = choiceInterval
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

pInterval :: CharParser () Interval
pInterval = (Interval . read) <$> many1 digit
    <|> pIntervalName

pExplicitIntervals :: CharParser () [Interval]
pExplicitIntervals = char '{' *> sepBy1 pInterval (char ',') <* char '}'

templateValue :: CharParser () a -> CharParser () (TemplateOption a)
templateValue p = (maybe Any OneOf) <$> (optionMaybe oneOrManyV)
    where 
        oneV = (: []) <$> p
        manyV = char '[' *> sepBy1 p (char ',') <* char ']'
        oneOrManyV = manyV <|> oneV

parseChordTemplate :: String 
    -> String 
    -> Either ParseError (ChordTemplate NoteTemplate [Interval])
parseChordTemplate = parse pChord
