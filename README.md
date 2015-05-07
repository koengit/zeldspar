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
  * https://github.com/emilaxelsson/feldspar-io

Set up a cabal sandbox in your Zeldspar repo, then install all the above
dependencies *in one go*:

    cabal install ../imperative-edsl/ ../feldspar-compiler ../feldspar-language/ ../feldspar-compiler-shim/ ../feldspar-io/

Install any other dependencies using `cabal install --only-dependencies`.

Now you're good to go. Have fun!
