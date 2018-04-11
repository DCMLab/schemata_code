module Schemas

import Base: ==, hash, collect, length, show, size, getindex
import DigitalMusicology.PitchOps: pc, transposeby, transposeto
import DigitalMusicology.PitchCollections: refpitch
using DigitalMusicology
using FunctionalCollections: plist, tail, head

export FlatSchema, stages, nstages, nvoices
export matchschema

# Schema Interface
# ================

"""
    stages(schema)

Returns a the stages of `schema`. 
"""
function stages end

"""
    nstages(schema)

Returns the number of stages in `schema`.
"""
function nstages end

"""
    nvoices(schema)

Returns the number of voices in `schema`.
"""
function nvoices end

"""
    matchschema(schema, gram)

Returns all reference pitch classes on which `gram` matches `schema`.
`gram` should be a vector of pitch vectors in voice order (highest to lowest).
"""
function matchschema end

"""
    schema_matches_from(schema, gram, ref)

Tests whether a schema based on the pitch `ref` matches
`gram`, which is a vector of pitch vectors in voice order (highest to lowest).
"""
function schema_matches_from end


# Flat Schema - the most simple representation of a schema
# ========================================================

"""
    FlatSchema(pitches)

Represents a schema as a matrix of pitch classes:
Rows are schema stages, columns are descending voices.
A three-voiced schema with four stages is then represented by a 4x3 matrix.
"""
struct FlatSchema{P} <: PitchCollection{P}
    pitches :: Array{P,2}

    FlatSchema(pitches::Array{P,2}) where P =
        new{P}(pc.(pitches))
end

### Schema interface implementations

getstage(fs::FlatSchema{P}, stage::Int) where P = view(fs.pitches, stage, :)

(stages(fs::FlatSchema{P}) :: Vector{Vector{P}}) where P =
    [getstage(fs, i) for i in size(fs.pitches, 1)]

nstages(fs::FlatSchema) = size(fs.pitches, 1)

nvoices(fs::FlatSchema) = size(fs.pitches, 2)

### Base implementations

==(fs1::FlatSchema{P}, fs2::FlatSchema{P}) where P =
    fs1.pitches == fs2.pitches

hash(fs::FlatSchema, x::UInt) = hash(fs.pitches, x)

size(fs::FlatSchema) = size(fs.pitches)
size(fs::FlatSchema, dims...) = size(fs.pitches, dims...)

getindex(fs::FlatSchema, i, j) = fs.pitches[i,j]

collect(fs::FlatSchema) = fs.pitches

show(io::IO, fs::FlatSchema) = write(io, "FlatSchema:", string(fs.pitches))

function show(io::IO, ::MIME"text/plain", fs::FlatSchema)
    ps = fs.pitches
    write(io, "Schema: ",
          string(size(ps, 1)), " stages, ",
          string(size(ps, 2)), " voices\n")
    for ri in 1:size(ps, 1)
        write(io, string(ri), ": ", join(fs.pitches[ri,:], " "), "\n")
    end
end

### Schema Matching

function stage_matches_from(sstage::AbstractArray{P,1}, gstage::Vector{P}, ref::P) where P
    nv = length(sstage)
    # check all voices
    v = 1
    for pitch in gstage
        pitch :: P
        # if current voice is matched...
        if pc(pitch) == pc(sstage[v] + ref)
            v += 1 # go to next voice
            if v > nv return true end
        end
    end
    v > nv # all found?
end

function schema_matches_from(fs::FlatSchema{P}, gram::Vector{Vector{P}}, ref::P) where P
    # check all stages
    all(1:nstages(fs)) do s
        stage_matches_from(getstage(fs, s), gram[s], ref)
    end
end

schemainst_stagepitches(inststage) =
    # sorts by default, specialize if this is not desired
    sort(pitches(inststage), rev=true)

# merge with general matchschema
function matchschema(fs::FlatSchema{P}, gram::Vector{Vector{P}}) where P
    # test compatibility
    if nstages(fs) != length(gram) return P[] end
    if any(g -> length(g) < nvoices(fs), gram) return P[] end
    
    # try all pcs as reference
    filter(ref -> schema_matches_from(fs, gram, ref), allpcs(P))
end

# match schemas on slice grams
#matchschema(fs::FlatSchema{P}, gram::Vector{Slice{N,Vector{P}}}) where {N,P} =
#    matchschema(fs, unwrapslices(gram))

# match schemas on unsims
matchschema(fs::FlatSchema{P}, gram) where P =
    matchschema(fs, map(schemainst_stagepitches, gram))

function reversepartialmatch(fs::FlatSchema{P}, pfx::plist{T}) where {P,T}
    stages = length(pfx)
    any(allpcs(P)) do ref
        schemastage = stages + 1
        all(pfx) do pfxstage
            schemastage -= 1
            stage_matches_from(getstage(fs, schemastage),
                               schemainst_stagepitches(pfxstage),
                               ref)
        end
    end
end

function schemaunsimskipgrams(itr, schemas::Vector{FlatSchema{P}}, k, n) where {P}
    uonset(u::Vector{TimedNote{P,T}}) where T  = onset(u[1]) # unsims are sorted by onset
    uoffset(u::Vector{TimedNote{P,T}}) where T = maximum(map(offset, u)) # but not by offset
    unsimdist(u1::Vector{TimedNote{P,T}}, u2::Vector{TimedNote{P,T}}) where T = uonset(u2) - uoffset(u1)
    nooverlap(u1::Vector{TimedNote{P,T}}, u2::Vector{TimedNote{P,T}}) where T = uonset(u2) >= uoffset(u1)
    pred(pfx::plist{Vector{TimedNote{P,T}}}) where T =
        nooverlap(head(tail(pfx)), head(pfx)) &&
            any(s -> reversepartialmatch(s, pfx), schemas)
    skipgrams(itr, k, n, unsimdist, pred)
end

function findschemas(schemas::Dict{String,FlatSchema{P}}, grams) where P
    matches(schema, gram) = (gram, matchschema(schema, gram))
    match(gram) = filter((n, ms) -> !isempty(ms[2]),
                         Dict(name => matches(schema, gram)
                              for (name, schema) in schemas))
    mergematches!(acc, new) = merge!(push!, acc, new)
    mapreduce(match, mergematches!, Dict(name => [] for (name, schema) in schemas), grams)
end

## Example

#@midi prinner1 = FlatSchema([9 5; 7 4; 5 2; 4 0])

end # module Schemas
