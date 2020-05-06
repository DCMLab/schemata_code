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

# # Loading Procedure
# Use the following steps to load and preprocess the data
# 1. Load the lexicon:  lex = Polygrams.loadlexicon(path)
# 2. Load the raw data: df, notelists = loadcorpusdata(corpusdir, schemaids)
# 3. Clean the data:    df = cleancorpusdata(df, lex)
# 4. Find groups:       df = findgroups(df)
# 5. (optional) Resample/balance the data:
#                       dfu = upsample(df)
#                       dfd = downsample(df)
# # Dataframe Format
# After loading, cleaning, and assigning groups,
# the dataframe has the following layout:
# Rows: one row per polygram, matched or annotated
# Columns:
# - piece (String): id of the piece the polygram is taken from
# - schema (String): id of the used schema variant
# - beatfactor (Int): Int the denominator of the piece's time signature (proxy for duration of a "beat")
# - notesraw (Vector^2{Note}: the unprocessed polygram as loaded (no missings but ragged stages)
# - notes (Vector^2{Note?}): the resolved polygram (stages of equal size, missing notes at correct place)
# - isinstance (Bool): is the polygram an actual schema instance or not?
# - overlap (Int?): ID of an instance the polygram overlaps with (which has the same ID in this column)
# - overcat (Symbol): :inst, :noinst, or :overlap (if non-instance but overlaps with instance)
# - group (Int): ID of transitively closed overlap group for same piece/schema
# - groupisinstance (Bool): does the "group" contain an instance?
# `notelists` is a dictionary from piece IDs to the corresponding note lists.

# Loading raw data
# ----------------

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

    polydf = DataFrame(notesraw=matches, isinstance=isinstance, beatfactor=beatfactors,
                       piece=piececol, schema=schemacol)
    
    return polydf, notes
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
    notelists = Dict()
    
    @showprogress 1 "loading pieces..." for piece in pieces
        #@info "loading piece $(piece)"
        polydf, notes = loadpiecedata(corpusdir, piece, schemaids)

        if df == nothing
            df = polydf
        else
            append!(df, polydf)
        end

        notelists[piece] = notes
    end

    return df, notelists
end

# Data cleaning
# -------------

"""
    hasmissings(instance)

Helper function to check if a schema instance is missing notes.
"""
hasmissings(inst) = length(Set(map(length,inst))) > 1

"""
    shuffledf(df)

Returns a new dataframe like `df` but with shuffled rows.
"""
shuffledf(df) = df[shuffle(1:(size(df)[1])), :]

"""
    cleancorpusdata(df, lexicon)

Takes a dataframe with corpus data and a schema lexicon.
Performs some post-processing on the dataframe, such as resolving implicit notes:
- Adds a new column `notes` that contains the resolved polygrams.
- Removes rows with unresolvable polygrams.
- Shuffles the rows' order.
Returns the new dataframe (the original is not modified)
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
    findgroups(df)

Takes a clean dataframe and finds groups of overlapping matches.
Returns a new dataframe with additional columns.

"Overlaps" are polygrams in the same piece and for the same schema
that temporally overlap.
"Groups" are all transitively closed (wrt. overlap) sets of polygrams.
in the same piece and for the same schema.

Returns a new dataframe with four new columns:
- `overlap`: for instances an ID,
  and for non-instances either the ID of an instance it overlaps with
  or `missing` if it doesn't overlap with any instance.
- `overcat`: `:inst` for instances,
  `:overlap` for non-instances overlapping with an instance,
  and `:noinst` for non-instances that don't overlap with any instance.
- `group`: a group ID that's shared between all polygrams that transitively overlap.
- `groupisinstance`: `true` if the group contains an instance, `false` if not. 
"""
function findgroups(df)
    nsteps = length(levels(df.schema)) * length(levels(df.piece))

    # find groups that overlap with true instances
    progmeter = Progress(nsteps, 0.1, "finding overlaps...")
    idoffset = 0
    
    function assignoverlaps(subdf)
        instances = subdf.notes[subdf.isinstance]

        overlapids = map(subdf.notes) do x
            oid = findfirst(inst -> Polygrams.polyssharetime(inst,x), instances)
            isnothing(oid) ? missing : oid + idoffset
        end
        
        idoffset = idoffset + length(instances)
        next!(progmeter)
        (notes=subdf.notes, overlap=overlapids)
    end
    
    odf = by(df, [:schema, :piece], [:notes, :isinstance] => assignoverlaps)
    df = join(df, odf, on=[:schema, :piece, :notes])

    # assign "categories": instance, overlaps with instance, non-instance (doesn't overlap)
    categories = map(df.isinstance, df.overlap) do inst, grp
        if ismissing(grp)
            :noinst
        elseif !inst
            :overlap
        else
            :inst
        end
    end
    df[!, :overcat] = CategoricalArray{Symbol}(categories)

    # grouping matches generally
    progmeter = Progress(nsteps,0.1, "assigning groups...")
    groupoffset = 0

    function grouppolys(subdf)
        polys = subdf.notes
        #println("computing groups for $(subdf.schema[1]) in $(subdf.piece[1]).")
        groups = Polygrams.transitiverel(Polygrams.polyssharetime, polys)
        groupdict = Dict()
        for (i, group) in enumerate(groups)
            for poly in group
                groupdict[poly] = i + groupoffset
            end
        end
        groupoffset = groupoffset + length(groups)
        next!(progmeter) # update the progress 
        (notes=subdf.notes, group=map(p -> groupdict[p], polys))
    end
    
    gdf = by(df, [:schema, :piece], [:notes, :schema, :piece] => grouppolys)
    df = join(df, gdf, on=[:schema, :piece, :notes])

    instgroups = Set(df[df.isinstance, :group])
    groupisinst = map(g -> g in instgroups, df.group)
    df[!, :groupisinstance] = groupisinst
    
    return df
end

# Resampling
# ----------

"""
    upsample(df)

Upsamples a dataframe to have a balanced isinstance column.
"""
function upsample(df, ratio=1)
    dftrues = df[df.isinstance, :]
    dffalses = df[.! df.isinstance, :]

    nf = size(dffalses)[1]
    nt = Int(round(nf * ratio))

    newtrues = dftrues[rand(1:size(dftrues)[1], nt), :]
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

    newfalses = dffalses[rand(1:size(dffalses)[1], nt), :]
    newdf = vcat(dftrues, newfalses)

    return shuffledf(newdf)
end

# Fetching contexts
# -----------------

"""
    findcontext(piece, notes)

Finds the "context" of a set of notes in a piece.
Returns all notes in `piece` that overlap with `notes`,
except `notes` themselves.
"""
function findcontext(piece, notes)
    notes = filter(!ismissing, notes)
    on = minimum(onset.(notes))
    off = maximum(offset.(notes))
    ids = id.(notes)
    return [note for note in piece if !((onset(note) >= off) | (offset(note) <= on) | (id(note) in ids))]
end

function findfullcontexts(df, notelists)
    df = copy(df)
    contexts = @showprogress 0.1 "finding contexts..." map(df.piece, df.notes) do piece, poly
        findcontext(notelists[piece], [n for stage in poly for n in stage])
    end
    df[!, :context] = contexts
    df
end

# Evaluating the features
# =======================

# Independent features
# --------------------

DigitalMusicology.duration(::Missing) = 0
DigitalMusicology.onset(::Missing) = missing
DigitalMusicology.offset(::Missing) = missing
DigitalMusicology.pitch(::Missing) = missing
DigitalMusicology.tointerval(::Missing) = missing

include("features.jl")

feats = Dict(
    :dur => getDuration,
    :vdist => (notes, _) -> Polygrams.voicedist(notes),
    :sskip => stageSkip,
    :rdsums => rhythmDistanceSumInEvent,
    :rdsumv => rhythmDistanceSumInVoice,
    :rhytreg => rhythmicirregularity,
    #:rstagetrans => rhythmStageTransitionDistSum,
    #:rvoicetrans => rhythmVoiceTransitionDistSum,
    :pdsums => (notes, _) -> pitchDistanceSumInEvent(notes),
    :pdsumv => (notes, _) -> pitchDistanceSumInVoice(notes),
    :preg => (notes, _) -> pitchirregularity(notes),
    #:pstagetrans => (notes, _) -> pitchStageTransitionDistSum(notes),
    #:pvoicetrans => (notes, _) -> pitchVoiceTransitionDistSum(notes),
)

"""
    runfeatures(df, features)

Takes a dataframe of schema instances and a dictionary of features.
The feature dictionary contains feature functions
that take a schema instance (with potentially missing notes)
and the beat length of the piece (which may be ignored).
The keys of the dictionary are keywords indicating the name of the feature

The features are evalueated on the `notes` and `barlen` columns of the dataframe
and added to it with the dictionary keys indicating the new column names.
Returns the extended dataframe.
"""
function runfeatures(df, features)
    df = copy(df)
    for (name, f) in features
        @info "evaluating feature $name" 
        df[!, name] = map(f, df.notes, df.beatfactor)
    end
    return df
end

# Dependent features (depend on training data)
# --------------------------------------------

"""
    trainfeatures(traindf)

Trains dependent features on a training set.
Returns a named tuple of trained info.
"""
function trainfeatures(traindf)
    ctxhists = contexthists(traindf[traindf.isinstance, [:notes, :context, :schema]])

    ctxprofiles = schemahists(ctxhists)

    return (ctxprofiles=ctxprofiles,)
end

"""
    rundepfeatures(df, info)

Runs features on `df` that depend on trained `info`,
as returned by `trainfeatures`.
Returns a new dataframe with extra columns.
"""
function rundepfeatures(df, info)
    ctxhists = contexthists(df)
    ctxfeat = contextfeature(ctxhists, info.ctxprofiles)
    df = join(df, ctxfeat[!, [:notes, :profiledist]], on=:notes)

    return df
end

featcols = [:profiledist, keys(feats)...]

# Plotting features
# -----------------

function plotcol(df, col; group=:overcat, kwargs...)
    density(df[:, col]; group=df[:, group], kwargs...)
end

function plotfeatures(df, cols; group=:overcat, width=600, height=150, title="feature distribution")
    n = length(cols)

    ps = [plotcol(df, c; group=group, title="$title $c", legend=(c==:vdist ? :best : :none)) for c in cols]
    
    plot(ps..., layout=grid(n,1), size=(width, height*n))
end

function plotfeatures(df, features, schema; group=:overcat, width=600, height=150)
    if schema == :all
        schemas = levels(df.schema)
        l = @layout grid(1, length(schemas))
        subplots = [plotfeatures(df, features, s) for s in schemas]
        plot(subplots..., layout = l,
             width=width,
             height=height*length(schemas)*length(features))
    else
        plotfeatures(df[df.schema .== schema, :], features, title=schema)
    end
end

# Running the model
# =================

function fitmodel(df, cols)
    form = term(:isinstance) ~ foldl(+, term.(cols))
    @show form
    glm(form, df, Bernoulli(), LogitLink())
end

function addpredictions(df, model)
    df = copy(df)
    df[!, :pred] = predict(model, df)
    df[!, :predbool] = df.pred .> 0.5
    df
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

function countbypiece(df)
    by(df, [:piece, :schema], ntrue = :isinstance=>sum, npred = :predbool=>sum)
end
