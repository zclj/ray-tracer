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

- [Book page](http://www.raytracerchallenge.com/)
- [Arrays](https://wiki.haskell.org/Arrays)
- [Vectors](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial)
- [RTC Haskell](https://github.com/micahcantor/haskell-raytracer)
- [Forum - Reflection](https://forum.raytracerchallenge.com/thread/4/reflection-refraction-scene-description)

## 3D models

- [MIT Graphics Course](https://groups.csail.mit.edu/graphics/classes/6.837/F03/models/)
- [Utah Graphics Course](https://graphics.cs.utah.edu/courses/cs6620/fall2013/?prj=5)
- [Triangle mesh normalization](https://forum.raytracerchallenge.com/thread/27/triangle-mesh-normalization)
