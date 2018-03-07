using Revise
using DigitalMusicology
using DigitalMusicology.Helpers.witheltype

using DataFrames

import FunctionalCollections
using IterTools: imap

FC = FunctionalCollections

uselac("/home/chfin/Uni/phd/data/midi_archive/");

notelist_span(ns::Vector{TimedNote{P,T}}) where {P,T} =
    maximum(map(offset, ns)) - minimum(map(onset, ns))

unsims(notes, maxioi, n) =
    skipgrams(notes, Float64(maxioi), n, onsetcost, stable=true)

function unsimskipgrams(unsims, k, n)
    uonset(u)  = onset(u[1]) # unsims are sorted by onset
    uoffset(u) = maximum(map(offset, u)) # but not by offset
    unsimdist(u1, u2) = uonset(u2) - uoffset(u1)
    nooverlap(pfx) = uonset(FC.head(pfx)) >= uoffset(FC.head(FC.tail(pfx)))
    skipgrams(unsims, k, n, unsimdist, nooverlap)
end

schemacanify(pitches::Vector{Vector{P}}) where {P<:Pitch} =
    

function schemacandidates(notes, voices, k1, stages, k2)
    sgs = unsimskipgrams(unsims(notes, k1, voices), k2, stages)
    witheltype(imap(sg -> schemacanify(map(pitches, sg))), Vector{Vector{MidiPitch}})
end

@midi prinner1 = FlatSchema([9 5; 7 4; 5 2; 4 0])

countschemamatches(fs::FlatSchema, itr) =
    sum(gram -> matchschema(fs, gram), itr)

function findfirstschema(fs::FlatSchema, itr)
    for gram in itr
        matches = matchschema(fs, gram)
        if !isempty(matches)
            return (gram, matches)
        end
    end
end

prinnerpiece = "1/mo-ps-03"

notes = getpiece(prinnerpiece, :notes_secs)

mozunsims = unsims(notes, 1.0, 2)

sgs = unsimskipgrams(mozunsims, 1.0, 2)

itrlen(itr) = count(x->true, itr)

function firstduplicate(itr)
    seen = Set{eltype(typeof(itr))}()
    for x in itr
        if x ∈ seen
            seenv = collect(seen)
            dup = findfirst(y->x==y, seenv)
            return (x, seenv[dup])
        end
        push!(seen, x)
    end
end

iscompat(u1, u2) = maximum(map(offset, u1)) <= minimum(map(offset, u2))
