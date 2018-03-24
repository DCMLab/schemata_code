module Unsims

using DigitalMusicology
using DigitalMusicology.Helpers.witheltype

import FunctionalCollections
#using IterTools: imap
FC = FunctionalCollections

include("trie2.jl")
using .SchemaTrie

export countpiecesschemas, countpiecesschemas, rankcounts, topranks

unsims(notes, maxioi, n, p=1.0) =
    skipgrams(notes, Float64(maxioi), n, onsetcost, stable=true, p=p)

function unsimskipgrams(unsims, k, n, p=1.0)
    uonset(u)  = onset(u[1]) # unsims are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    unsimdist(u1, u2) = uonset(u2) - uoffset(u1)
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    skipgrams(unsims, k, n, unsimdist, nooverlap, p=p)
end

# read output as:: [1][1] is ref (always 0), rest is pc relative to rest
# order of groups = temporal, order of notes = original pitch ascending
function schemacandify(notes::Vector{Vector{N}}) where {N<:Note}
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

# function schemacandidates(notes, voices, k1, stages, k2)
#     sgs = unsimskipgrams(unsims(notes, k1, voices), k2, stages)
#     witheltype(imap(sg -> schemacandify(map(pitches, sg))), Vector{Vector{MidiPitch}})
# end

function countmapby(itr, mapf=id, weightf=(x->1.0))
    T = typeof(mapf(first(itr))) # ugly hack
    coll = mktrie(eltype(T), Float64)
    #coll = Dict{T, Float64}()
    for x in itr
        mapped = mapf(x)
        update!(x -> x + weightf(x), coll, mapped, 0.0)
        #coll[mapped] = get(coll, mapped, 0.0) + weightf(x)
    end
    coll
end

countschemas(itr) = countmapby(itr, schemacandify)

function countpieceschemas(id, voices, k1, stages, k2, p)
    println("processing ", id)
    notes = getpiece(id, :notes_secs)
    sgs = unsimskipgrams(unsims(notes, k1, voices), k2, stages, p)
    counts = countschemas(sgs)
    println("done with  ", id)
    counts
end

function countpiecesschemas(pieces, voices, k1, stages, k2, p)
    counts(id) = Unsims.countpieceschemas(id, voices, k1, stages, k2, p)
    reduce((a,b) -> merge!(+, a, b),
           pmap(counts, pieces))
end

rankcounts(counts) = sort(collect(counts), rev=true, by=x->x[2])

function candidatestring(candidate)
    stages = map(candidate) do stage
        string(map(Int, stage))
    end
    join(stages, " -> ")
end

printrank(rank) =
    print(string(rank[2], ": ", candidatestring(rank[1])), "\n")

topranks(ranks, n) =
    foreach(printrank, Iterators.take(ranks, n))

# """
#     findfirstsg(uitr, pattern, k, n, [p], [matchfn])

# Enumerates skipgrams until the first match of `pattern` is found.
# """

function findfirstusg(uitr, pattern, k::Float64, n::Int, p=0.1^(n-1))
    pat = collect(pattern)
    subpats = map(1:length(pat)) do i
        pat[1:i]
    end
    uonset(u)  = onset(u[1]) # unsims are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    unsimdist(u1, u2) = uonset(u2) - uoffset(u1)
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    function prefixpred(pfx::FC.plist{T}) where T
        #@show typeof(pfx)
        l = length(pfx)
        pfxcand = schemacandify(reverse(collect(T,pfx)))
        nooverlap(pfx) && pfxcand == subpats[l]
    end
    first(skipgrams(uitr, k, n, unsimdist, prefixpred, p=p))
end

end
