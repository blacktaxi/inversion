#!/bin/sh
./dist/build/inversion/inversion "$@" -o=Midi | python preview/preview-chords.py