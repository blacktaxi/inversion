Inversion
=========
`inversion.exe` is a chord search assistant for stringed instruments with
frets. It is a command line tool that takes an instrument configuration and chord
specification and outputs all possible chords in a rank-based order.

Command line argument syntax and most of the functionality is not stable yet,
stay tuned for updates.

Currently it can generate chords for an E-standard tuned guitar and for ukulele
in standard tuning. There's also a separate tool that can play the chords using
a MIDI output device.

Generate and pretty-print top 5 chords:
-   with a G root at any octave
-   with mandatory 0 and 4th semitone intervals
-   optional 7th, 12th, 11th and 16th
-   on a ukulele
```
$ inversion.exe -c=G{=0,4,7?,12?,11?,16?}
  1-A |---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|
  2-E |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  3-C |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  4-g O---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18

  1-A |---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|
  2-E |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  3-C |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  4-g |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18

  1-A |---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|---|---|
  2-E |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  3-C |---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|
  4-g O---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18

  1-A |---|-O-|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
  2-E |---|---|-O-|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
  3-C |---|-O-|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
  4-g O---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18

  1-A |---|---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|
  2-E |---|---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|---|
  3-C |---|---|---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|
  4-g |---|---|---|---|---|---|---|---|---|---|-O-|---|---|---|---|---|---|---|
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
```

Similarly, for guitar, but output results in JSON:
```
$ inversion.exe -i=guitar -c=A{=0,4,7?,12?,11?,14?} -o=Json
{"instrument":{"frets":21,"strings":{"1-e":["E",5],"2-B":["B",4],"3-G":["G",4],"4-D":["D",4],"5-A":["A",3],"6-E":["E",3]}},"chords":[{"6-E":5,"5-A":4,"1-e":0,"3-G":2,"4-D":6,"2-B":0},{"5-A":0,"3-G":6,"1-e":0,"4-D":6,"2-B":0},{"5-A":0,"3-G":6,"6-E":0,"4-D":6,"2-B":0},{"5-A":0,"3-G":6,"1-e":0,"4-D":7,"2-B":0},{"5-A":0,"3-G":6,"6-E":0,"4-D":7,"2-B":0}]}
```

Generate chords with *any* root, but with specified intervals. Output results
as lists of MIDI notes and pipe that to the script that plays chords:
```
$ inversion.bat -c=*{0,3,7,9} -o=Midi | playchords.bat
[67, 60, 75, 69]
[78, 60, 64, 69]
[67, 60, 69, 75]
[67, 60, 75, 81]
[79, 60, 75, 69]
```

See more options with `inversion.bat -?`.