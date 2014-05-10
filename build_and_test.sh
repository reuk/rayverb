cabal install --ghc-options="-rtsopts -fforce-recomp -Wall -prof -auto-all" -fdebug --enable-executable-profiling
rayverb ~/Desktop/boom.wav +RTS -K100M -h -p -sstderr
hp2ps -c rayverb.hp
open rayverb.ps
