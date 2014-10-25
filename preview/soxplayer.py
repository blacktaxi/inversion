'''This implementation uses sox for playback. On OS X, you can install sox with `brew install sox`.
'''
import math

def notenum_to_freq(notenum):
    """Converts MIDI note number to frequency."""
    f0 = 440.0
    a = 2 ** (1.0 / 12.0)
    return f0 * (a ** (notenum - 69)) # 69 is A-4 in MIDI

def waveform(t):
    return 0.3 * (t - float(int(t / math.pi)) * math.pi) + (0.7 * math.sin(t))

def render_chord(notes, sample_rate):
    voices = []
    bolt = [0, 0.0]

    def tick_voices():
        # fade out
        for v in voices:
            v[1] *= 0.99999

    def tick():
        bolt[0] += 1
        t = float(bolt[0]) / float(sample_rate) * math.pi * 2
        bolt[1] = sum(waveform(t * freq) * amp for freq, amp in voices)
        tick_voices()

    for n in notes:
        voices.append([notenum_to_freq(n), 1.0 / len(notes) * 0.5])

    for _ in xrange(sample_rate * 3):
        tick()
        yield bolt[1]

def output_chords(chords, sample_rate, f):
    import struct
    for c in chords:
        for w in render_chord(c, sample_rate):
            f.write(struct.pack('h', max(-32767, min(32767, int(w * 32767.0)))))

def play_chords(chords):
    import sys
    import subprocess
    
    play = subprocess.Popen(['play', '-ts16', '-r44100', '-c1', '-'], stdin=subprocess.PIPE)
    output_chords(chords, 44100, play.stdin)
    play.stdin.close()
    play.wait()
