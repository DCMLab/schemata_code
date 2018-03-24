addprocs([("chfin-tp", :auto)])

addprocs()

#using Revise
using DigitalMusicology

# using DataFrames

#uselac("/home/chfin/Uni/phd/data/midi_archive/");
@everywhere DigitalMusicology.usekern("/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/");

@everywhere include("skipgrams.jl");

# notelist_span(ns::Vector{TimedNote{P,T}}) where {P,T} =
#     maximum(map(offset, ns)) - minimum(map(onset, ns))


# @midi prinner1 = FlatSchema([9 5; 7 4; 5 2; 4 0])

# countschemamatches(fs::FlatSchema, itr) =
#     sum(gram -> matchschema(fs, gram), itr)

# function findfirstschema(fs::FlatSchema, itr)
#     for gram in itr
#         matches = matchschema(fs, gram)
#         if !isempty(matches)
#             return (gram, matches)
#         end
#     end
# end

prinnerpiece = "sonata03-2"

notes = getpiece(prinnerpiece, :notes_secs)

mozunsims = unsims(notes, 1.0, 2, 1.0)

sgs = unsimskipgrams(mozunsims, 1.0, 4, 0.00001)

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

hasrepeat(cand) = any(i -> cand[i] == cand[i+1], 1:(length(cand)-1))
