#!/bin/bash

cd ..
cabal-dev install --force-reinstalls

cd Test
# ghc --make -o Test -hide-all-packages -no-user-package-conf -package-conf ../cabal-dev/packages-7.2.2.conf -package-id base-4.4.1.0-1f81e4e7baf87af8ebbcf52b7bec2254 -package-id `(cabal-dev -s ../cabal-dev clean && cabal-dev -s ../cabal-dev configure -p && cabal-dev -s ../cabal-dev buildopts) 2>/dev/null | grep ^rbm` -package-id repa-2.2.0.1-f99aadecee8a3c011b366742dc64de18 -O2 -prof -rtsopts -auto-all -threaded Test1.hs
ghc --make -o Test -hide-all-packages -no-user-package-conf -package-conf ../cabal-dev/packages-7.2.2.conf -package-id base-4.4.1.0-1f81e4e7baf87af8ebbcf52b7bec2254 -package-id `(cabal-dev -s ../cabal-dev clean && cabal-dev -s ../cabal-dev configure -p && cabal-dev -s ../cabal-dev buildopts) 2>/dev/null | grep ^rbm` -package-id repa-2.2.0.1-f99aadecee8a3c011b366742dc64de18 -O2 -rtsopts -threaded Test1.hs

for n in 1 2 3 4
do
    echo N = $n
    time ./Test +RTS -N$n
done
