#!/bin/sh
cd src
./Main.hs "$@" -o=Midi | ./Preview.hs | play -ts16 -r44100 -c1 -x -