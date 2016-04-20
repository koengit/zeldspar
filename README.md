# zeldspar
Ziria + Feldspar = sant

## Installation

Here is a suggested incantation:

    git clone git@github.com:emilaxelsson/raw-feldspar
    git clone git@github.com:koengit/zeldspar
    cd zeldspar
    cabal sandbox init
    cabal sandbox add-source ../raw-feldspar
    cabal install --constraint="language-c-quote -full-haskell-antiquotes"

