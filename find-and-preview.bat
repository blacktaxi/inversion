@dist\build\inversion\inversion.exe %*
@dist\build\inversion\inversion.exe %* -j > tmp
@preview\preview-chords.py < tmp