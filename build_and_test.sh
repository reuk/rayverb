cabal install --ghc-options="-rtsopts -fforce-recomp -Wall -prof -auto-all" -fdebug --enable-executable-profiling
# cabal install --ghc-options="-rtsopts -Wall -prof -auto-all" -fdebug --enable-executable-profiling 
# tracer ~/Desktop/trace2.json +RTS -h -p -sstderr
flattener ~/Desktop/trace2.json ~/Desktop/out.wav +RTS -h -p -sstderr
hp2ps -c tracer.hp
hp2ps -c flattener.hp
