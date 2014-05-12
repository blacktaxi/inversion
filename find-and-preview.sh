#!/bin/sh
./dist/build/inversion/inversion "$@" -o=Midi | ./dist/build/audio-preview/audio-preview | play -ts16 -r44100 -c1 -x -