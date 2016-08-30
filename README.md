# Zeldspar [![Build Status](https://travis-ci.org/kmate/zeldspar.svg?branch=master)](https://travis-ci.org/kmate/zeldspar)
Ziria + Feldspar = sant

## Installation

Here is a suggested incantation:

    git clone git@github.com:koengit/zeldspar
    cd zeldspar
    cabal sandbox init
    cabal install

You can get a faster install and a smaller sandbox by changing the last line to:

    cabal install --constraint="language-c-quote -full-haskell-antiquotes"

(Do this only when installing in a sandbox that has no other dependencies on `language-c-quote`.)
