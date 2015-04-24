# zeldspar
Ziria + Feldspar = sant

Building
--------

Check out the following repositories:

  * Zeldspar itself
  * https://github.com/emilaxelsson/imperative-edsl/
  * https://github.com/feldspar/feldspar-language
  * https://github.com/feldspar/feldspar-compiler
  * https://github.com/emwap/feldspar-compiler-shim

In `feldspar-compiler`, check out the `internal_shape` branch.

Set up a cabal sandbox in your Zeldspar repo, then install all the above
dependencies *in one go*:

    cabal install ../feldspar-compiler-shim/ ../imperative-edsl/ ../feldspar-compiler ../feldspar-language/

Install any other dependencies using `cabal install --only-dependencies`.

Now you're good to go. Have fun!
