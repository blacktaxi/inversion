module TemplateParse (parseChordTemplate) where

import Text.ParserCombinators.Parsec
import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))

import Note (ABC (..), Octave (..))
import Interval (Interval (..))
import Template

-- <chord> :: <root_note>["("<octave_range>")"](<explicit_intervals>|<chord_spec>)
-- <octave_range> :: \d|(\d,\d,...)|(\d-\d)
-- <explicit_intervals> :: "*"\d,\d,\d,...
-- <chord_spec> :: ...

chordTemplate :: CharParser () (ChordTemplate NoteTemplate [Interval])
chordTemplate =	do
	rootNote <- Exact <$> note
	rootOctave <- exactOrAny octave
	intervals <- intervals
	return $ ChordTemplate (NoteTemplate rootNote rootOctave) intervals

note :: CharParser () ABC
note = do
	n <- oneOf "ABCDEFG"
	m <- maybeToList <$> (optionMaybe (oneOf "sb"))
	return $ read ([n] ++ m)

octave :: CharParser () Octave
octave = do
	_ <- char '('
	o <- oneOf "012345678"
	_ <- char ')'
	return $ Octave (read [o])

intervals :: CharParser () [Interval]
intervals = do
	_ <- char '*'
	is <- sepBy1 (many1 digit) (char ',')
	return $ (Interval . read) <$> is

exactOrAny :: CharParser () a -> CharParser () (TemplateOption a)
exactOrAny p = (maybe Any Exact) <$> (optionMaybe p)

parseChordTemplate :: String -> String -> Either ParseError (ChordTemplate NoteTemplate [Interval])
parseChordTemplate = parse chordTemplate