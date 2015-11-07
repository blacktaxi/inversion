#!/usr/bin/env stack
-- stack --resolver lts-3.7 --install-ghc runghc
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.Int (Int16 (..))
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (ByteString, concat, putStr)

type Amplitude = Double
type Time = Double

type Signal = Time -> Amplitude

mix :: Signal -> Signal -> Signal
mix x y = (+) <$> x <*> y

amp :: Signal -> Signal -> Signal
amp x y = (*) <$> x <*> y

silence :: Signal
silence = const 0

slice :: Time -> Time -> Signal -> Signal
slice startT endT s = \t -> if t <= endT then s (t + startT) else 0.0

trunc :: Time -> Signal -> Signal
trunc = slice 0.0

delay :: Time -> Signal -> Signal
delay t s = \x -> if x >= t then s (x - t) else 0.0

sine :: Time -> Signal
sine freq = \t -> sin $ freq * (2.0 * pi) * t

saw :: Time -> Signal
saw freq t = (1.0 - frac) * 2.0 - 1.0
    where (_, frac) = properFraction (t * freq)

triangle :: Time -> Signal
triangle freq t =
    if odd i
        then (1.0 - frac) * 2.0 - 1.0
        else frac * 2.0 - 1.0
    where (i, frac) = properFraction (t * freq)

square :: Time -> Signal
square freq t = if odd i then 1.0 else -1.0
    where (i, _) = properFraction (t * freq)

volume :: Amplitude -> Signal -> Signal
volume x s = (* x) <$> s

fade :: Time -> Signal
fade speed = exp . (* speed) . (* (-1.0))

midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n = f0 * (a ** (fromIntegral n - midiA4))
    where
        f0 = 440.0
        a = 2 ** (1.0 / 12.0)
        midiA4 = 69

niceNote :: Int -> Signal
niceNote n = mix voice1 voice2
    where
        voice1 = amp (volume 0.1 $ fade 1.9) (square (midiNoteToFreq n))
        voice2 = amp (volume 0.2 $ fade 4.0) (sine (midiNoteToFreq n))

chord :: [Int] -> Signal
chord notes =
    foldr mix silence noteWaves
    where
        noteWaves = zipWith (\i n -> delay (fromIntegral i * 0.05) $ niceNote n) [0..] notes

clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high s = max low . min high . s

render :: Time -> Time -> Int -> Signal -> [Int16]
render startT endT sampleRate s =
    [asInt16 (sample * sampleTime) | sample <- [0..totalSamples]]
    where
        clipped = (clip (-1.0) 1.0 s) :: Time -> Amplitude
        sampleTime = 1.0 / (fromIntegral sampleRate)
        minSig = fromIntegral (minBound :: Int16)
        maxSig = fromIntegral (maxBound :: Int16)
        toInt16 x = (truncate (minSig + ((x + 1.0) / 2.0 * (maxSig - minSig))))
        asInt16 = (toInt16 <$> clipped) :: Time -> Int16
        totalSamples = (endT - startT) / sampleTime

main :: IO ()
main = do
    chordStrings <- fmap lines getContents
    forM_ chordStrings $ \s -> do
        BS.putStr $ playChord s
    where
        playChord notesStr = BS.concat $ map encode (render 0.0 3.5 44100 sound)
            where
                notes = (read notesStr) :: [Int]
                sound = chord notes
