# Schemata

This repository contains Julia code for the schema project.
To run it, the [DigitalMusicology.jl](https://github.com/DCMLab/DigitalMusicology.jl) library is needed.
You can install it from the Julia REPL using
```
julia> Pkg.clone("https://github.com/DCMLab/DigitalMusicology.jl.git")
```

* `src/polygrams.jl` provides the main functionality for handling schemata using skipgrams over skipgrams.
* `src/scratch.jl` serves as a scratchpad for ideas, functionality that is in development, and boilerplate code for development.
  Not necessarily up to date, might be broken.
* `notebooks/polygrams.ipynb` contains an example
