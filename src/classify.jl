# classify.jl

using Random
using DigitalMusicology
using DataFrames
using GLM
using ProgressMeter
using Plots; gr()
using StatsPlots

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
    beatfactor = convert(Float64, denominator(content(timesigs[1][1])))

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
                #@warn "error loading $(schemaid) for $(pieceid)"
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
    
    @showprogress 1 "loading pieces..." for piece in pieces
        #@info "loading piece $(piece)"
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

function findgroups(df)
    groupcol = missings(Union{Missing,Int}, size(df)[1])

    instances = df[df.isinstance, :]
    @showprogress 0.1 "assigning groups..." for (group, inst) in enumerate(eachrow(instances))
        for (i, x) in enumerate(eachrow(df))
            if ((inst.schema == x.schema) & (inst.piece == x.piece)
                & Polygrams.polyssharetime(inst.notes, x.notes))
                if !ismissing(groupcol[i])
                    @warn "ambiguous group assignment"
                end
                groupcol[i] = group
            end
        end
    end
    
    df = copy(df)
    df[!, :group] = groupcol

    df[!, :category] = df.isinstance .+ (.! ismissing.(df.group))

    return df
end

shuffledf(df) = df[shuffle(1:(size(df)[1])), :]

"""
    cleancorpusdata(df, lexicon)

Takes a dataframe with corpus data and a schema lexicon.
Performs some post-processing on the dataframe, such as resolving implicit notes.
"""
function cleancorpusdata(df, lexicon)
    notescol = map(df.notesraw, df.schema, df.piece) do notes, schema, piece
        try
            Polygrams.resolveimplicits(notes, lexicon[schema])
        catch e
            @warn "excludingincorrect instance for $schema in $piece:\n$e"
            missing
        end
    end
    df = copy(df)
    df[!, :notes] = notescol

    df = df[(!ismissing).(df.notes), :]

    return shuffledf(df)
end

"""
    upsample(df)

Upsamples a dataframe to have a balanced isinstance column.
"""
function upsample(df, ratio=1)
    dftrues = df[df.isinstance, :]
    dffalses = df[.! df.isinstance, :]

    nf = size(dffalses)[1]
    nt = Int(round(nf * ratio))

    newtrues = dftrues[sample(1:size(dftrues)[1], nt), :]
    newdf = vcat(dffalses, newtrues)

    return shuffledf(newdf)
end

"""
    downsample(df)

Downsamples a dataframe to have a balanced isinstance column.
"""
function downsample(df, ratio=1)
    dftrues = df[df.isinstance, :]
    dffalses = df[.! df.isinstance, :]

    nt = size(dftrues)[1]
    nf = Int(round(nt * ratio))

    newfalses = dffalses[sample(1:size(dffalses)[1], nt), :]
    newdf = vcat(dftrues, newfalses)

    return shuffledf(newdf)
end

# Evaluating the features
# =======================

DigitalMusicology.duration(::Missing) = 0
DigitalMusicology.onset(::Missing) = missing
DigitalMusicology.offset(::Missing) = missing
DigitalMusicology.pitch(::Missing) = missing
DigitalMusicology.tointerval(::Missing) = missing

include("features.jl")

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

feats = Dict(
    :dur => getDuration,
    :vdist => (notes, _) -> Polygrams.voicedist(notes),
    :sskip => stageSkip,
    :rdsums => rhythmDistanceSumInEvent,
    :rdsumv => rhythmDistanceSumInVoice,
    :rhytreg => rhythmicirregularity,
)

# plotting features
# -----------------

function plotfeatures(df, features; group=:category)
    cols = collect(keys(feats))
    n = length(cols)

    ps = [density(df[:, c], group=df[:, group]) for c in cols]
    
    plot(ps..., layout=n)
end

# Running the model
# =================

function fitmodel(df, features)
    form = term(:isinstance) ~ foldl(+, term.(keys(features)))
    glm(form, df, Bernoulli(), LogitLink())
end

function addpredictions!(df, model)
    df.pred = predict(model, df)
    df.predbool = df.pred .> 0.5
end

function showeval(df)
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
    println("recall:\t\t", recall)
    println("f1-score:\t", 2 * precision*recall / (precision + recall))
    println("MCC:\t\t", (tp * tn - fp * fn) / sqrt((1.0*tp+fp) * (tp+fn) * (tn+fp) * (tn+fn))) 
end
