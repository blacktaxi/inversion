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
import sys

def note_to_int(note):
    abc = note[0]
    octave = note[1]

    abcs = ['C', 'Cs', 'D', 'Ds', 'E', 'F', 'Fs', 'G', 'Gs', 'A', 'As', 'B']
    abcnum = (i for i, n in zip(xrange(12), abcs) if n == abc).next()

    return octave * 12 + abcnum

def chord_to_notes(string_notes, chord):
    oc = chord.items()
    oc.sort(key=lambda x: x[0])
    return [string_notes[k] + v for k, v in oc[::-1]]

def chords_from_json(jsonstr):
    import json
    x = json.loads(jsonstr)

    strings = x['instrument']['strings']
    chords = x['chords']

    string_notes = {k: note_to_int(v) for k, v in strings.items()}
    return [chord_to_notes(string_notes, c) for c in chords]

if __name__ == '__main__':
    input_data = sys.stdin.read()

    if len(sys.argv) > 1 and sys.argv[1] == 'json':
        chords = chords_from_json(input_data)
    else:
        chords = map(eval, input_data.splitlines())

    try:
        from pygameplayer import play_chords
        play_chords(chords)
    except:
        from soxplayer import play_chords
        play_chords(chords)
