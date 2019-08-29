# Schemata

## Introduction

Voice-leading schemata are important building blocks in tonal music.
In the context of this project they are characterized as sets of voices
that move in a coordinated way through a number of stages.
On the musical surface, this structure can be ornamented and elaborated,
so a major challenge is to discover (and/or annotate)
the skeleton of a schema in the music.

## Setup

This repository contains Julia code and notebooks for the schema project.
To use it, clone this repo and install the dependencies.
This can be done by running `julia` inside its top-level directory,
pressing `]` to enter the package manager mode, and typing

``` julia-repl
(v1.x) pkg> activate .
(schemata_code) pkg> instantiate
```

The first command activates the repo's *environment*.
The second command downloads all dependencies such as
[DigitalMusicology.jl](https://github.com/DCMLab/DigitalMusicology.jl)
in the version specified in this package.

Some of the notebooks require a
[separate corpus](https://github.com/DCMLab/corpora/tree/master) (branch `schemata`).
The git repository of that corpus should be cloned to `data/dcml_corpora` by default.
Keeping the corpus in a different location requires adapting some paths
in the annotation notebooks.

``` shell
$ cd data
$ git clone git@github.com:DCMLab/corpora.git dcml_corpora
$ cd dcml_corpora
$ git checkout schemata
```

Alternatively, a fork of that repo can be used in combination with pull requests.
If you don't use a fork (and have commit access to the corpus repo),
then add annotations on a feature branch that you push to the repo
and use a pull request in order to get your branch merged.


## Code Structure

* `src/polygrams.jl` provides basic functionality for handling schemata
  and serves as an entry point.
* `src/matcher.jl` contains a schema matcher based on nested skipgrams (polygrams).
* `src/autoannot.jl` contains [Interact](https://github.com/JuliaGizmos/Interact.jl)
  widgets for creating semi-automatic schema annotations.
* `src/io.jl` contains I/O code for schema annotations and lexica.
* `src/counting.jl` contains older code for counting occurrences
  of certain classes of polygrams.
* `deps/` contains JS code for some of the widgets.
* `app/rundemoapp.jl` is a starter for a web-app (dysfunctional)
* `app/prepare_corpus.jl` is a script that adds note IDs
  to all MusicXML files in a directory.

### Notebooks

These are the most important notebooks in the `notebooks` directory:

  * `Mozart Annotation.ipynb` is preconfigured for adding annotations
    for the Mozart piano sonatas.
  * `Review Annotations.ipynb` allows to review and correct annotations
    (set to Mozart piano sonatas by default, WIP).
  * `Interactive Annotation.ipynb` demonstrates the usage
    of the interactive annotation widgets.
