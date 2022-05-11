# ray-tracer

## Executing

Create the pre-configured demo canvas with `stack run`. `canvas.ppm` will be created with the resulting canvas.

## Dev

use `ghcid` for continuously getting compile feedback

use `stack repl` to start REPL

## Tests

To continuously run the tests, use:

`stack test --file-watch`

## Profiling

First, build with profiling `stack build --profile`. Then run with profiling `stack exec --profile -- ray-tracer-exe +RTS -p`.

## Resources

- [Arrays](https://wiki.haskell.org/Arrays)
- [Vectors](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial)
