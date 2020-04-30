# classify.jl

using DigitalMusicology
using DataFrames
using GLM
using MLBase

#include("common.jl")
#include("io.jl")
include("Polygrams.jl")

# Loading the data
# ================

"""
    loadpiecedata(corpusdir, pieceid, schemaids)

Loads positive and negative instances of the schema `schemaid`
for the piece `pieceid` in the corpus `corpusdir`.
Returns a tuple of positive instances (`Array`),
negative instances (`Array`) and bar length of the piece (`Rational{Int}`).

`corpusdir` points to the directory that contains `musicxml/`,
`annotations/`, and `groups/`.
`schemaids` is a list of schema IDs as defined in the schema lexicon.
"""
function loadpiecedata(corpusdir, pieceid, schemaids)
    # load notes
    usedir(joinpath(corpusdir, "musicxml"))

    notes = getpiece(pieceid, :notes, :musicxml, type=:notes, keepids=true, unfold=true)
    timesigs = getpiece(pieceid, :timesigs, :musicxml, nowarn=true)
    beatfactor = denominator(content(timesigs[1][1]))

    notedict = Polygrams.mknotedict(notes)
    total = 0

    matches = Vector{Vector{TimedNote{SpelledInterval,Rational{Int}}}}[]
    isinstance = Bool[]
    schemacol = CategoricalArray{String}(undef, 0)

    for schemaid in schemaids
        try
            groups = Polygrams.loadgroups(pieceid, schemaid, joinpath(corpusdir, "groups"), notedict, pick=true)
            suggested = [inst for group in groups for inst in group]
            positives = Polygrams.loadannots(pieceid, schemaid, joinpath(corpusdir, "annotations"), notedict, pick=true)

            negatives = setdiff(suggested, positives)
            
            append!(matches, positives)
            append!(isinstance, trues(length(positives)))
        
            append!(matches, negatives)
            append!(isinstance, falses(length(negatives)))

            both = length(positives) + length(negatives)
            total = total + both
            append!(schemacol, fill(schemaid, both))
        catch e
            if isa(e, SystemError) # mainly because file doesn't exist
                @warn "error loading $(schemaid) for $(pieceid)"
            else
                throw(e)
            end
        end
    end

    beatfactors = fill(beatfactor, total)
    piececol = CategoricalArray{String}(undef, total)
    fill!(piececol, pieceid)
    
    return DataFrame(notesraw=matches, isinstance=isinstance, beatfactor=beatfactors,
                     piece=piececol, schema=schemacol)
end

"""
    loadcorpusdata(corpusdir, schemaids)

Loads positive and negative instances for a whole corpus and a given schema.
Returns a data frame with three columns:
- `match`: The matched notes as a nested array of `TimedNote`s
- `instance`: A boolean indicating if the match is a positive (`true`) or negative (`false`) example
- `barlen`: The duration of one bar according to the time signature of the piece the match is taken from.


`corpusdir` points to the directory that contains `musicxml/`,
`annotations/`, and `groups/`.
`schemaids` is a list of schema IDs as defined in the schema lexicon.
"""
function loadcorpusdata(corpusdir, schemaids)
    usedir(joinpath(corpusdir, "musicxml"))
    pieces = allpieces()
    df = nothing
    
    for piece in pieces
        @info "loading piece $(piece)"
        piecedf = loadpiecedata(corpusdir, piece, schemaids)

        if df == nothing
            df = piecedf
        else
            append!(df, piecedf)
        end
    end

    return df
end

"""
    hasmissings(instance)

Helper function to check if a schema instance is missing notes.
"""
hasmissings(inst) = length(Set(map(length,inst))) > 1


"""
    cleancorpusdata!(df, lexicon)

Takes a dataframe with corpus data and a schema lexicon.
Performs some post-processing on the dataframe, such as resolving implicit notes.
"""
function cleancorpusdata!(df, lexicon)
    notescol = map(df.notesraw, df.schema, df.piece) do notes, schema, piece
        try
            Polygrams.resolveimplicits(notes, lexicon[schema])
        catch e
            @warn "incorrect instance for $schema in $piece: $e"
            missing
        end
    end
    df.notes = notescol

    return df[(!ismissing).(df.notes), :]
end

# Evaluating the features
# =======================

"""
    runfeatures!(df, features)

Takes a dataframe of schema instances and a dictionary of features.
The feature dictionary contains feature functions
that take a schema instance (with potentially missing notes)
and the beat length of the piece (which may be ignored).
The keys of the dictionary are keywords indicating the name of the feature

The features are evalueated on the `notes` and `barlen` columns of the dataframe
and added to it with the dictionary keys indicating the new column names.
"""
function runfeatures!(df, features)
    for (name, f) in features
        @info "evaluating feature $name" 
        df[!, name] = map(f, df.notes, df.beatfactor)
    end
end

DigitalMusicology.duration(::Missing) = 0
DigitalMusicology.pitch(::Missing) = missing
DigitalMusicology.tointerval(::Missing) = missing

feats = Dict(
    :dur => (notes, beatf) -> 1.0 * beatf * Polygrams.totalduration(notes),
    :vdist => (notes, _) -> Polygrams.voicedist(notes),
    :sskip => (notes, beatf) -> 1.0 * beatf * Polygrams.instageskip(notes)
)

# Running the model
# =================

function fitmodel(df, features)
    glm(@formula(isinstance ~ dur + vdist + sskip), df, Bernoulli(), LogitLink())
end

function addpredictions!(df, model)
    df.pred = predict(model, df)
    df.predbool = df.pred .> 0.5
end

function evaluation(df)
    gttrue = df.isinstance .== true
    gtfalse = df.isinstance .== false
    predtrue = df.predbool .== true
    predfalse = df.predbool .== false

    tp = count(gttrue .& predtrue)
    tn = count(gtfalse .& predfalse)
    fp = count(gtfalse .& predtrue)
    fn = count(gttrue .& predfalse)
    
    println("confusion matrix:")
    println("true positives:  ", tp)
    println("true negatives:  ", tn)
    println("false positives: ", fp)
    println("false negatives: ", fn)

    println()

    println("accuracy:\t", (tp + tn) / length(df.isinstance))

    precision = tp / (tp + fp)
    recall = tp / (tp + fn)

    println("precision:\t", precision)
    println("recall:\t", recall)
    println("f1-score:\t", 2 * precision*recall / (precision + recall))
end
