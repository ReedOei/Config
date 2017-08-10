ghc -prof -fprof-auto -rtsopts ./$1
./$1 +RTS -p
