# classify.jl

using DigitalMusicology
using DataFrames

include("io.jl")


"""
    loadpiecedata(corpusdir, pieceid, schemaid)

Loads positive and negative instances of the schema `schemaid`
for the piece `pieceid` in the corpus `corpusdir`.
Returns a tuple of positive instances (`Array`),
negative instances (`Array`) and bar length of the piece (`Rational{Int}`).

`corpusdir` points to the directory that contains `musicxml/`,
`annotations/`, and `groups/`.
`schemaid` corresponds to the schema IDs defined in the schema lexicon.
"""
function loadpiecedata(corpusdir, pieceid, schemaid)
    # load notes
    try
        usedir(joinpath(corpusdir, "musicxml"))

        notes = getpiece(pieceid, :notes, :musicxml, type=:notes, keepids=true, unfold=true)
        timesigs = getpiece(pieceid, :timesigs, :musicxml, nowarn=true)

        notedict = mknotedict(notes)

        groups = loadgroups(pieceid, schemaid, joinpath(corpusdir, "groups"), notedict, pick=true)
        suggested = [inst for group in groups for inst in group]
        positives = loadannots(pieceid, schemaid, joinpath(corpusdir, "annotations"), notedict, pick=true)

        negatives = setdiff(suggested, positives)
        return positives, negatives, duration(content(timesigs[1][1]))
    catch e
        if isa(e, SystemError) # mainly because file doesn't exist
            @warn "error loading $(pieceid)"
            return nothing
        else
            throw(e)
        end
    end
end

"""
    loadcorpusdata(corpusdir, schemaid)

Loads positive and negative instances for a whole corpus and a given schema.
Returns a data frame with three columns:
- `match`: The matched notes as a nested array of `TimedNote`s
- `instance`: A boolean indicating if the match is a positive (`true`) or negative (`false`) example
- `barlen`: The duration of one bar according to the time signature of the piece the match is taken from.


`corpusdir` points to the directory that contains `musicxml/`,
`annotations/`, and `groups/`.
`schemaid` corresponds to the schema IDs defined in the schema lexicon.
"""
function loadcorpusdata(corpusdir, schemaid)
    usedir(joinpath(corpusdir, "musicxml"))
    pieces = allpieces()

    matches = Vector{Vector{TimedNote{SpelledInterval,Rational{Int}}}}[]
    barlens = Rational{Int}[]
    isinstance = Bool[]
    
    for piece in pieces
        @info "loading piece $(piece)"
        piecedata = loadpiecedata(corpusdir, piece, schemaid)
        if isnothing(piecedata)
            continue
        end
        pos, neg, barlen = piecedata

        append!(matches, pos)
        append!(barlens, fill(barlen, length(pos)))
        append!(isinstance, trues(length(pos)))
        
        append!(matches, neg)
        append!(barlens, fill(barlen, length(neg)))
        append!(isinstance, falses(length(neg)))
    end

    return DataFrame(match=matches, instance=isinstance, barlen=barlens)
end

"""
    hasmissings(instance)

Helper function to check if a schema instance is missing notes.
"""
hasmissings(inst) = length(Set(map(length,inst))) > 1
