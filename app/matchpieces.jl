#!/usr/bin/env julia
import Pkg; Pkg.activate(joinpath(@__DIR__, ".."))
using ArgParse

Base.CoreLogging.disable_logging(Base.CoreLogging.Warn)

s = ArgParseSettings()
@add_arg_table s begin
    "--trim", "-t"
    help = "the number of alternatives to keep per match"
    arg_type = Int
    default = 100
    "--lexicon", "-l"
    help = "path to the lexicon.json file"
    default = "lexicon.json"
    "--out", "-o"
    help = "the output will be saved to <dir>/<out>/<schema>/piece.json"
    default = "groups"
    "--in", "-i"
    help = "the input directory will be <dir>/<in>"
    default = "musicxml"
    "--force", "-f"
    help = "re-match existing files, even if the input file hasn't changed"
    action = :store_true
    "dir"
    help = "the corpus directory"
    required = true
    "schema"
    help = "the name of the schema to be matched"
    required = true
end

opts = parse_args(s)
dir = opts["dir"]
schema = opts["schema"]

println("Generating matches for $(schema) in $(dir)")

include("../src/Polygrams.jl")
using DigitalMusicology

# TODO: add incremental build + force option

"""
    matchpiece(piece, pattern)

Matches a single piece with a single pattern.
Returns a list of match groups, in which the matches overlap.
In each group, matches are pre-sorted by heuristics.
"""
function matchpiece(piece, pattern; trim=nothing)
    # load data
    notes = getpiece(piece, :notes, :musicxml, type=:notes, keepids=true)
    xml = getpiece(piece, :xml, :musicxml, keepids=true)
    timesigs = getpiece(piece, :timesigs, :musicxml)[1]
    pattern = lexicon[schema]
    
    # prepare score function
    timesig = content(timesigs[1])
    barlen = duration(timesig)
    beatfactor = denominator(timesig)
    featurefs = [
        poly -> beatfactor * Polygrams.totalduration(poly), # duration
        Polygrams.voicedist,                                # voice dist
        poly -> beatfactor * Polygrams.instageskip(poly)    # stage skip
        #, poly -> Polygrams.polymweight(poly, timesigs) # metric weight
    ]
    featureweights = [1, -1, -2] # maaaaaagic
    
    # run the matcher
    print("\tmatching")
    polyitr = Polygrams.schemamatches(notes, [pattern], barlen, barlen)
    polys = collect(polyitr)
    print(" ($(length(polys)))")
    print("\tsorting")
    sorted = Polygrams.sortbyheuristics(polys, featurefs, featureweights)
    # scores = Dict(poly => Polygrams.rate(poly, featurefs, featureweights) for poly in polys)

    # find groups
    print("\tfiltering")
    best = Polygrams.bestmatchestime(sorted)
    print(" ($(length(best)))")
    print("\tgrouping")
    groups = map(b -> Polygrams.findcompetitors(Polygrams.polyssharetime, b, sorted), best)

    if trim != nothing
        groups = map(g -> g[1:min(trim,length(g))], groups)
    end

    groups
end

"""
    matchcorpus(corpusdir, outdir, schema, pattern)

Matches `pattern` in every piece in `corpusdir`.
Saves the output to `outdir/schema`.
"""
function matchcorpus(corpusdir, outdir, schema, pattern; trim=nothing, force=false)
    usedir(corpusdir)
    for piece in allpieces()
        print("$(piece)...")

        # check if up to date
        infile = getpiece(piece, :file, :musicxml)
        outfile = Polygrams.annotfilename(piece, schema, outdir)
        if !isfile(outfile) || mtime(infile) > mtime(outfile) || force
            groups = matchpiece(piece, pattern; trim=trim)
            sort!(groups, by=g -> onset(g[1][1][1]))
            print("\twriting")
            Polygrams.savegroups(piece, schema, groups, outdir)
            println("\tdone.")
        else
            println(" already up to date.")
        end
    end
end

# main
corpusdir = joinpath(dir, opts["in"])
outdir = joinpath(dir, opts["out"])

lexicon = Polygrams.loadlexicon(opts["lexicon"])
pattern = lexicon[schema]

matchcorpus(corpusdir, outdir, schema, pattern; trim=opts["trim"], force=opts["force"])
