#!python
'''This script can preview (play) chords that are output from inversion.

It accepts JSON in the format like this:
    {
        "instrument": {
            "frets":15,
            "strings":{"1-e":["E",4],"2-B":["B",3],"3-G":["G",3],"4-D":["D",3],"5-A":["A",2],"6-E":["E",2]}
        },
        "chords": [
            {"6-E":0,"5-A":0,"1-e":0,"3-G":0,"4-D":0,"2-B":0}
        ]
    }

You can get this JSON from inversion by using JSON output mode.
'''
from pygame import midi
import sys
import time
import json

def note_to_int(note):
    abc = note[0]
    octave = note[1]

    abcs = ['C', 'Cs', 'D', 'Ds', 'E', 'F', 'Fs', 'G', 'Gs', 'A', 'As', 'B']
    abcnum = (i for i, n in zip(xrange(12), abcs) if n == abc).next()

    return (octave + 1) * 12 + abcnum

def chord_to_notes(string_notes, chord):
    oc = chord.items()
    oc.sort(key=lambda x: x[0])
    return [string_notes[k] + v for k, v in oc[::-1]]

def play_chord(out, notes):
    def play(dly):
        # use different channel for each note/string
        # useful when some strings resolve to the same note
        # sounds better
        for cn, n in zip(xrange(666), notes):
            out.note_on(n, 100, cn + 1)
            time.sleep(dly)

    play(0.25)
    time.sleep(0.7)
    play(0.025)
    time.sleep(2)

def init_output_device():
    out = midi.Output(midi.get_default_output_id())

    # set all channels to guitar
    for c in xrange(15):
        out.set_instrument(24, c + 1)

    return out

if __name__ == '__main__':
    x = sys.stdin.read()
    x = json.loads(x)

    strings = x['instrument']['strings']
    chords = x['chords']

    string_notes = {k: note_to_int(v) for k, v in strings.items()}

    midi.init()
    out = init_output_device()
    for c in chords:
        ns = chord_to_notes(string_notes, c)
        print ns
        play_chord(out, ns)
    midi.quit()