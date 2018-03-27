addprocs([("chfin-tp", :auto)])
#addprocs([("128.179.133.166", :auto)])

addprocs()

using DigitalMusicology

# using DataFrames

#uselac("/home/chfin/Uni/phd/data/midi_archive/");
@everywhere DigitalMusicology.usekern("/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/");

@everywhere include("skipgrams.jl")

include("skipgrams.jl")

function weightpieces(pieces)
    densities = map(allpieces()) do id
        notes = getpiece(id, :notes_secs)
        (id, length(notes)/(onset(notes[end])-onset(notes[1])))
    end
    mdens = mean(map(x->x[2], densities))
    map(densities) do p
        (p[1], mdens/p[2])
    end
end

function piecebarlen(piece)
    fn = piecepath(piece, "midi-norep", ".mid")
    midi = MIDI.readMIDIfile(fn)
    foreach(MIDI.toabsolutetime!, midi.tracks)
    uptrack = MidiFiles.uptype(midi.tracks[1])
    sig = map(tsme -> tsme.ev.sig, filter(ev -> typeof(ev.ev) == MidiFiles.TimeSignatureME, uptrack))[1]
    sig.num//sig.denom
end

pieces = map(p -> (p, piecebarlen(p)), allpieces())

@time counts = Unsims.countpiecesschemasbars(pieces, 3, 4, 1.0, 0.000000001);

ranks = Unsims.rankcounts(counts);

Unsims.topranks(ranks, 50)

@time counts = Unsims.countpieceschemaswholes("sonata03-1", 3, 0.5, 4, 0.5, 0.00001, 0.1)

# 03-2
# 3x4, p=1e-8: 9833, 185s
# 3x4, p=1e-9: 270, 113s
# 2x4, p=1e-5: 30000, 11s
# 03-1
# 2x4, p=1e-5: 2e6, 600s
# 3x4, p=1e-10: DEATH
# 3x4, p=1e-7, p1=0.1: 8625, 54s
# 3x4, p=1e-6, p1=0.1: 88833, 114s
# 3x4, p=1e-6, p1=0.2: 1.2e6, 600s
# 3x4, p=1e-4, p1=0.1: 8e6, 1900s
# 3x4, p=1e-5, p1=0.1: 8e5, 509s

@time counts = Unsims.countpiecesschemasbars(pieces, 2, 4, 0.00001)

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

hasmorethanthree(cand) = length(Set(vcat(cand...))) > 3
