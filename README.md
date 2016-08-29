# Zeldspar [![Build Status](https://travis-ci.org/kmate/zeldspar.svg?branch=master)](https://travis-ci.org/kmate/zeldspar)
Ziria + Feldspar = sant

## Installation

Here is a suggested incantation:

    git clone git@github.com:koengit/zeldspar
    cd zeldspar
    cabal sandbox init

When installing dependencies in a sandbox, you can get a faster experience and a smaller sandbox by passing a constraint for `language-c-quote`:

    cabal install --only-dependencies --constraint="language-c-quote -full-haskell-antiquotes" zeldspar
