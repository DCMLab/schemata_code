module Polygrams

using DigitalMusicology
using DigitalMusicology.Helpers: witheltype
import MIDI

using MusicologyPlots
import VegaLite

using DataFrames: DataFrame
import FileIO
import JSON

import FunctionalCollections
using FunctionalCollections: plist, EmptyList

using LinearAlgebra: dot
#using GLM

# internal dependencies

include("helpers.jl")

export verticals, horizontals, schemarep, piecebarlen
export findfirsthoriz
export savepolys, loadpolys, polystodf

# generate polygrams
# ==================

verticals(notes, maxioi, n, p=1.0) =
    skipgrams(notes, Float64(maxioi), n, onsetcost, stable=true, p=p)

uonset(u)  = onset(u[1]) # verticals are sorted by onset
uoffset(u) = maximum(map(offset, u)) # but not by offset

nooverlap(pfx) = uonset(FunctionalCollections.head(pfx)) >= uoffset(FunctionalCollections.head(FunctionalCollections.tail(pfx)))

groupdist(k) = function(u1, u2)
    dist = onset(u2[1]) - onset(u1[1])
    dist > k ? (k+1) : 0
end

function horizontals(verticals, k, n, p=1.0)
    skipgrams(verticals, k, n, groupdist(k), nooverlap, p=p)
end

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

function piecebarlen(piece, corpus=getcorpus())
    fn = piecepath(piece, "midi-norep", ".mid", corpus) # specific to mozart corpus
    midi = MIDI.readMIDIfile(fn)
    foreach(MIDI.toabsolutetime!, midi.tracks)
    uptrack = MidiFiles.uptype(midi.tracks[1])
    sig = map(tsme -> tsme.ev.sig, filter(ev -> typeof(ev.ev) == MidiFiles.TimeSignatureME, uptrack))[1]
    sig.num//sig.denom
end

"""
    findfirsthoriz(vitr, pattern, k, [p])

Enumerates horizontals over verticals until the first match of `pattern` is found.
"""
function findfirsthoriz(vitr, pattern, k::Float64, p=0.1^(length(pattern)-1))
    pat = collect(pattern)
    n = length(pat)
    subpats = map(1:length(pat)) do i
        pat[1:i]
    end
    
    function prefixpred(pfx::plist{T}) where T
        l = length(pfx)
        pfxcand = schemarep(reverse(collect(T,pfx)))
        nooverlap(pfx) && pfxcand == subpats[l]
    end

    first(skipgrams(vitr, k, n, groupdist(k), prefixpred, p=p))
end

polynotes(poly) = vcat(poly...)

polyrange(poly) = let
    lower = onset(poly[1][1])
    upper = maximum(map(offset, poly[end]))
    (lower, upper)
end

# do things with polygrams:

include("counting.jl")

include("matcher.jl")

include("autoannot.jl")

include("io.jl")

end # module
