module TemplateParse (parseChordTemplate) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (*>), (<*))

import Note (ABC (..), Octave (..))
import Interval (Interval (..))
import Template

-- <chord> :: <root_note>["("<octave_range>")"](<explicit_intervals>|<chord_spec>)
-- <octave_range> :: \d|(\d,\d,...)|(\d-\d)
-- <explicit_intervals> :: "{"\d,\d,\d,..."}"
-- <chord_spec> :: <chord_quality><interval_num><altered_fifth><additional_interval>
-- <chord_quality> :: "m" | "M" | ""
-- <interval_num> :: "(" ("maj" | "min" | "aug" | "dim")<number> ")"

chord :: CharParser () (ChordTemplate NoteTemplate [Interval])
chord = do
    note' <- templateValue note
    octave' <- (maybe Any id) <$> optionMaybe (char '(' *> templateValue octave <* char ')')
    intervals' <- intervals
    return $ ChordTemplate (NoteTemplate note' octave') intervals'

note :: CharParser () ABC
note = do
    n <- oneOf "ABCDEFG"
    m <- maybe [] ((: []) . sharpOrFlat) <$> optionMaybe (oneOf "#b")
    return $ read ([n] ++ m)
    where 
        sharpOrFlat '#' = 's'
        sharpOrFlat x = x

octave :: CharParser () Octave
octave = Octave . read . (: []) <$> oneOf "012345678"

intervals :: CharParser () [Interval]
intervals = map (Interval . read) <$> 
    (char '{' *> sepBy1 (many1 digit) (char ',') <* char '}')

templateValue :: CharParser () a -> CharParser () (TemplateOption a)
templateValue p = (maybe Any OneOf) <$> (optionMaybe oneOrManyV)
    where 
        oneV = (: []) <$> p
        manyV = char '[' *> sepBy1 p (char ',') <* char ']'
        oneOrManyV = manyV <|> oneV

parseChordTemplate :: String 
    -> String 
    -> Either ParseError (ChordTemplate NoteTemplate [Interval])
parseChordTemplate = parse chord