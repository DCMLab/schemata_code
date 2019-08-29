#!/usr/bin/env julia
### prepare_corpus.jl indir outdir
### Prepare a directory of musicxml files.
### Reads each input xml and writes an output with newly assigned note IDs.

println("prepare_corpus.jl: loading...")

import Pkg; Pkg.activate(joinpath(@__DIR__, "../"))
using DigitalMusicology: loadwithids
using LightXML: save_file

indir = ARGS[1]
outdir = ARGS[2]
println("converting files from $indir")
println("to $outdir")

if !isdir(indir)
    error("directory does not exist: $indir")
end

for fn in readdir(indir)
    base, ext = splitext(fn)
    if ext != ".xml" && ext != ".musicxml"
        continue
    end

    xml = loadwithids(joinpath(indir, fn))
    outfn = joinpath(outdir, base * ".xml") # convert extension to .xml in any case
    save_file(xml, outfn)
    println("wrote $outfn")
end

println("done")
