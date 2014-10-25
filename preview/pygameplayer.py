'''This player uses pygame.midi for playback.
'''
from pygame import midi
import time

def play_chord(out, notes):
    # use different channel for each note/string
    # useful when some strings resolve to the same note
    # sounds better
    notes = zip(xrange(666), notes)
    def play(dly, speedup=False):
        for cn, n in notes:
            out.note_on(n, 100, cn + 1)
            time.sleep(dly)
            if speedup: dly = dly * 0.9

    play(0.2)
    time.sleep(0.7)
    play(0.04, True)
    time.sleep(2)

    for cn, n in notes: out.note_off(n, 100, cn + 1)
    time.sleep(0.1)

def init_output_device():
    out = midi.Output(midi.get_default_output_id())

    # set all channels to guitar
    for c in xrange(15):
        out.set_instrument(24, c + 1)

    return out

def play_chords(chords):
    midi.init()
    out = init_output_device()
    for ns in chords:
        print ns
        play_chord(out, ns)
    out.close()
    out.abort()
    try: midi.quit()
    except: pass
