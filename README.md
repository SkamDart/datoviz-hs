# datoviz-hs
In-progress Haskell bindings to [datoviz](https://github.com/datoviz/datoviz).

To run the single demo, run the following commands with nix.

```
nix develop
# TODO this should be automated in the future
# manually find the datoviz include path in LD_LIBRARY_PATH and run these two commands for now
glslc -o mandelbrot.frag.spv mandelbrot.frag
glslc -o mandelbrot.vert.spv mandelbrot.vert -I /nix/store/fsdinc3ikljgf7rbrvc1abyh9421p1mh-datoviz/include/datoviz/glsl/
cabal repl
mandelbrot
```

