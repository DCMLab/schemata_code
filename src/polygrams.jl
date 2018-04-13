module Polygrams

using DigitalMusicology
using DigitalMusicology.Helpers.witheltype
import MIDI

import FunctionalCollections
#using IterTools: imap
FC = FunctionalCollections

include("trie2.jl")
using .SchemaTrie

include("fsa.jl")
using .Automata

export verticals, horizontals
export schemarep, countmapby, countschemas
export countpiecesschemassecs, countschemassecs
export piecebarlen, countschemaswhole, countpiecesschemasbars
export rankcounts, topranks
export findfirsthoriz, schemamatches

# generate polygrams
# ==================

verticals(notes, maxioi, n, p=1.0) =
    skipgrams(notes, Float64(maxioi), n, onsetcost, stable=true, p=p)

function horizontals(verticals, k, n, p=1.0)
    uonset(u)  = onset(u[1]) # verticals are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    groupdist(u1, u2) = let dist = onset(u2[1]) - onset(u1[1])
        dist > k ? (k+1) : 0
    end
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    skipgrams(verticals, k, n, groupdist, nooverlap, p=p)
end

# count polygram schemata
# =======================

# transforms polygram to representant of schema equivalence class:
# [1][1] is the reference pitch (always 0), rest is pc relative to reference
# order of groups = temporal, order of notes = original pitch ascending
function schemarep(notes::Vector{Vector{N}}) where {N<:Note}
    cand = map(pitches, notes) # extract pitches from timed notes
    map(sort!, cand)           # sort pitch groups (lowest to highest)
    ref = cand[1][1]           # transpose -> reference pitch = 0, pc(...):
    for i in 1:length(cand)
        for j in 1:length(cand[i])
            cand[i][j] = pc(cand[i][j] - ref)
        end
    end
    cand
end

function countschemas(itr, weightf=(x->1.0))
    T = typeof(schemarep(first(itr))) # ugly hack
    coll = mktrie(eltype(T), Float64)
    for x in itr
        schema = schemarep(x)
        update!(x -> x + weightf(x), coll, schema, 0.0)
    end
    coll
end

## polygram schemata based on real time
## ------------------------------------

function countschemassecs(id, voices, k1, stages, k2, p)
    println("processing ", id)
    notes = getpiece(id, :notes_secs)
    sgs = horizontals(verticals(notes, k1, voices), k2, stages, p)
    counts = countschemas(sgs)
    println("done with  ", id)
    counts
end

function countpiecesschemassecs(pieces, voices, k1, stages, k2, p)
    counts(id) = Polygrams.countschemassecs(id, voices, k1, stages, k2, p)
    reduce((a,b) -> merge!(+, a, b),
           pmap(counts, pieces))
end

## polygram schemata based on note durations (fractions of wholes)
## ---------------------------------------------------------------

function piecebarlen(piece)
    fn = piecepath(piece, "midi-norep", ".mid") # specific to mozart corpus
    midi = MIDI.readMIDIfile(fn)
    foreach(MIDI.toabsolutetime!, midi.tracks)
    uptrack = MidiFiles.uptype(midi.tracks[1])
    sig = map(tsme -> tsme.ev.sig, filter(ev -> typeof(ev.ev) == MidiFiles.TimeSignatureME, uptrack))[1]
    sig.num//sig.denom
end

function countschemaswholes(id, voices, k1, stages, k2, p2, p1=1.0)
    println("processing ", id)
    notes = map(getpiece(id, :notes_wholes)) do note
        TimedNote{MidiPitch,Float64}(pitch(note), convert(Float64, onset(note)), convert(Float64, offset(note)))
    end
    sgs = horizontals(verticals(notes, convert(Float64, k1), voices, p1),
                         convert(Float64, k2), stages, p2)
    counts = countschemas(sgs)
    println("done with  ", id)
    counts
end

function countpiecesschemaswholes(pieces, voices, stages, p2, p1=1.0)
    counts(piece) = Polygrams.countschemaswholes(piece[1], voices, piece[2],
                                                   stages, piece[2], p2, p1)
    reduce((a,b) -> merge!(+, a, b),
           pmap(counts, pieces))
end

# analyze schema counts
# =====================

rankcounts(counts) = sort(collect(counts), rev=true, by=x->x[2])

function schemastring(candidate)
    stages = map(candidate) do stage
        string(map(Int, stage))
    end
    join(stages, " -> ")
end

printrank(rank) =
    print(string(rank[2], ": ", schemastring(rank[1])), "\n")

topranks(ranks, n) =
    foreach(printrank, Iterators.take(ranks, n))

# Matching Schemata
# =================

# """
#     findfirsthoriz(vitr, pattern, k, [p])

# Enumerates horizontals over verticals until the first match of `pattern` is found.
# """

function findfirsthoriz(vitr, pattern, k::Float64, p=0.1^(length(pattern)-1))
    pat = collect(pattern)
    n = length(pat)
    subpats = map(1:length(pat)) do i
        pat[1:i]
    end
    uonset(u)  = onset(u[1]) # verticals are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    groupdist(u1, u2) = uonset(u2) - uoffset(u1)
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    function prefixpred(pfx::FC.plist{T}) where T
        #@show typeof(pfx)
        l = length(pfx)
        pfxcand = schemarep(reverse(collect(T,pfx)))
        nooverlap(pfx) && pfxcand == subpats[l]
    end
    first(skipgrams(vitr, k, n, groupdist, prefixpred, p=p))
end

makefsa(schemas) = compile(words2nfa(schemas))

partialmatch(cand, fsa) = preaccepts(fsa, cand)

schemamatches(notes, schema::Vector{Vector{MidiPitch}}, k1, k2) =
    schemamatches(notes, [schema], k1, k2)

function schemamatches(notes, schemas::Vector{Vector{Vector{MidiPitch}}}, k1, k2, p2=1.0)
    nv = length(schemas[1][1]) # voices
    ns = length(schemas[1])    # stages

    if !all(schema -> all(s -> length(s) == nv, schema), schemas)
        error("All schemata must have the same number of voices")
    end
    if !all(schema -> length(schema) == ns, schemas)
        error("All schemata must have the same number of stages")
    end

    fsa = makefsa(schemas)

    uonset(u)  = onset(u[1]) # verticals are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    groupdist(u1, u2) = uonset(u2) - uoffset(u1)
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    function prefixpred(pfx::FC.plist{T}) where T
        pfxcand = schemarep(reverse(collect(T,pfx)))
        nooverlap(pfx) && partialmatch(pfxcand, fsa)
    end

    skipgrams(verticals(notes, k1, nv), Float64(k2), ns, groupdist, prefixpred, p=p2)
end

end # module
