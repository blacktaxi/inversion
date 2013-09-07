rem @dist\build\inversion\inversion.exe %*
@dist\build\inversion\inversion.exe %* -o=Midi > tmp
@preview\preview-chords.py < tmp