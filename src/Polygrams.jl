module Polygrams

using DigitalMusicology
using DigitalMusicology.Helpers: witheltype
import MIDI

using MusicologyPlots
#import VegaLite

using DataFrames: DataFrame
import FileIO
import JSON

import FunctionalCollections
using FunctionalCollections: plist, EmptyList

using LinearAlgebra: dot
#using GLM

# internal dependencies

include("helpers.jl")
include("common.jl")

export verticals, horizontals, schemarep, piecebarlen
export findfirsthoriz
export savepolys, loadpolys, savegroups, polystodf

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
    lower = minimum(onset(n) for n in poly[1] if !ismissing(n))
    upper = maximum(offset(n) for n in poly[end] if !ismissing(n))
    (lower, upper)
end

# do things with polygrams:

include("counting.jl")

include("matcher.jl")

include("autoannot.jl")

include("io.jl")

end # module
