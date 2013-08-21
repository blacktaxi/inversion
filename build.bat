runhaskell Setup.hs build
strip -p --strip-unneeded --remove-section=.comment -o dist/build/inversion/inversion.exe dist/build/inversion/inversion.exe