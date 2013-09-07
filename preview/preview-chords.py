from pygame import midi
import sys
import time
import json

def note_to_int(note):
    abc = note[0]
    octave = note[1]

    abcs = ['C', 'Cs', 'D', 'Ds', 'E', 'F', 'Fs', 'G', 'Gs', 'A', 'As', 'B']
    abcnum = (i for i, n in zip(xrange(12), abcs) if n == abc).next()

    return (octave + 2) * 12 + abcnum # FIX octave offset?

def chord_to_notes(string_notes, chord):
    return [string_notes[k] + v for k, v in chord.items()]

def play_chord(out, notes):
    for n in notes:
        out.note_on(n, 127, 1)
        time.sleep(0.025)
    time.sleep(2)

if __name__ == '__main__':
    x = json.loads(sys.stdin.read())

    strings = x['instrument']['strings']
    chords = x['chords']

    string_notes = {k: note_to_int(v) for k, v in strings.items()}

    midi.init()
    out = midi.Output(midi.get_default_output_id())
    for c in chords:
        ns = chord_to_notes(string_notes, c)
        print ns
        play_chord(out, ns)
    midi.quit()