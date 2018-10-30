#### counting.jl
#### enumerate polygrams and count occurrences of certain categories

include("trie.jl")
using .SchemaTrie

export countmapby, countschemas
export countpiecesschemassecs, countschemassecs
export countschemaswhole, countpiecesschemasbars
export rankcounts, topranks

# count polygram schemata
# =======================

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
