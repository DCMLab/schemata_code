#### matcher.jl
#### find schema instances in a piece

include("fsa.jl")
using .Automata

export schemamatches, matchpiece

# Matching Schemata
# =================

makefsa(schemas) = compile(words2nfa(schemas))

function relatives(ps)
    ref = ps[1]
    map(p -> ic(p-ref), ps[2:end])
end

schemamatches(notes, schema::Vector{Vector{Pitch}}, k1, k2) =
    schemamatches(notes, [schema], k1, k2)

function schemamatches(notes, schemas, k1, k2, p2=1.0)
    ns, nv = size(schemas[1])

    if !all(schema -> size(schema) == (ns, nv), schemas)
        error("All schemata must have the same dimensions")
    end

    # use automata to match verticals and horizontals according to schemas
    vertfsa = makefsa(map(relatives, vcat(map(collect ∘ eachrow,schemas)...)))
    horifsa = makefsa(map(collect∘eachrow, schemas))

    # predicate for horizontals: match and no overlap
    function horipred(pfx::plist{T}) where T
        # notes = permutedims(hcat(map(s -> map(pitch,s), reverse(collect(T,pfx)))...))
        # @show notes
        pfxcand = schemarep(reverse(collect(T,pfx)))
        nooverlap(pfx) && preaccepts(horifsa, eachrow(pfxcand))
    end

    # compute all verticals but filter for interval pattern matches
    verts = Iterators.filter(skipgrams(notes, Float64(k1), nv, onsetcost, stable=true)) do vert
        accepts(vertfsa, relatives(sort!((tointerval∘pitch).(vert))))
    end
    # compute horizontals and filter for schema matches on the fly
    skipgrams(verts, Float64(k2), ns, groupdist(k2), horipred, p=p2)
end

polyssharenotes(poly1, poly2) =
    !isempty(polynotes(poly1) ∩ polynotes(poly2))

function polyssharetime(poly1, poly2)
    l1, u1 = polyrange(poly1)
    l2, u2 = polyrange(poly2)
    !(l1 > u2 || l2 > u1)
end

function transitiverel(rel, items)
    P = eltype(items)
    Classes = EmptyList{plist{P}}
    classes = Classes()
    for item in items
        related = Classes()
        unreltd = Classes()
        for class in classes
            if any(c -> rel(c, item), class)
                related = cons(class, related)
            else
                unreltd = cons(class, unreltd)
            end
        end
        joint = cons(item, foldr(append, EmptyList{P}(), related))
        classes = cons(joint, unreltd)
    end
    #collect(map(collect, classes))
    map(collect, collect(classes))
end

transitiveoverlap(polys) = transitiverel(polyssharenotes, polys)

function sortbyweight(polys, weight)
    weighted = map(p->(p,weight(p)), polys)
    sort!(weighted, rev=true, by=x->x[2])
end

function bestmatches(compat, sortedpolys::Vector{P}) where {P}
    i = 0
    results = P[]
    for poly in sortedpolys
        if all(other -> compat(poly, other), sortedpolys[1:i])
            push!(results, poly)
        end
        i += 1
    end
    results
end

function bestmatchestime(sortedpolys::Vector{P}) where {P}
    disjoint((l1,u1), (l2,u2)) = u1 <= l2 || u2 <= l1
    strictdisjoint((l1,u1), (l2,u2)) = u1 < l2 || u2 < l1
    results = P[]
    covered = [] # time segments covered so far
    for poly in sortedpolys
        prange = polyrange(poly)
        # check if still free
        if all(crange -> disjoint(prange, crange), covered)
            push!(results, poly)
        end
        # merge into covered
        old = collect(filter(c -> strictdisjoint(c, prange), covered))
        overlapping = push!(collect(filter(c -> !strictdisjoint(c, prange), covered)), prange)
        lower = minimum(x[1] for x in overlapping)
        upper = maximum(x[2] for x in overlapping)
        covered = push!(old, (lower,upper))
    end
    results
end

findcompetitor(iscompet, poly, others) =
    others[findfirst(o -> iscompet(poly, o), others)]


findcompetitors(iscompet, poly, others) =
    filter(o -> iscompet(poly, o), others)

# heuristics
# ----------

totalduration(poly) =
    sum(stage -> sum(duration, stage), poly)

instageskip(poly) =
    sum(stage -> onsetcost(stage[1], stage[end]), poly)

# function sortschema(notes::Vector{Vector{N}}) where {N<:Note}
#     cand = map(s -> map(tointerval, pitches(s)), notes) # extract pitches from timed notes
#     map(sort!, cand)           # sort pitch groups (lowest to highest)
#     ref = cand[1][1]           # transpose -> reference pitch = 0, pc(...):
#     for i in 1:length(cand)
#         for j in 1:length(cand[i])
#             cand[i][j] = cand[i][j] - ref
#         end
#     end
#     cand
# end

function octdiv(int)
    octs = 0
    oct = octave(int)
    # count full octaves in the interval
    while int >= oct
        int -= oct
        octs += 1
    end
    # add another octave for >= fifths, since then going down is shorter
    if sign(ic(int)) == -1
        octs += 1
    end
    octs
end

function voicedist(poly)
    rep = schemarep(poly; toic=false)
    ns, nv = size(rep)
    sum(1:(ns-1)) do s
        sum(1:nv) do v
            octdiv(abs(rep[s+1,v] - rep[s,v]))
        end
    end
end

polymweight(poly, tsm) =
    sum(stage -> sum(note -> metricweight(onset(note), tsm), stage), poly)

rate(poly, fs, weights) =
    dot(map(f -> f(poly), fs), weights)

function sortbyheuristics(polys, fs, weights)
    ratingf(poly) = rate(poly, fs, weights)
    map(first, sortbyweight(polys, ratingf))
end

bestrated(polys, fs, weights) = bestmatches(!polyssharetime, sortbyheuristics(polys, fs, weights))

# fitting
# -------

function genfeatures(polys, keywords, functions)
    DataFrame(Dict(kw => f.(polys) for (kw,f) in zip(keywords,functions)))
end

# interactive matching
# --------------------

"""
    grouppolys(polys, schemadict)

Takes a list of polygrams and a dictionary from schema prototypes to ids.
Returns a dictionary from schema ids to lists of polygrams conforming to the corresponding prototype.
"""
grouppolys(polys, schemadict) = groupby(p -> schemadict[schemarep(p)], polys)

"""
    matchpiece(pieceid, schemata; params=[1, -1//12, -2], corpus=getcorpus())

Matches `schemata` on the notes of `pieceid`, using the parameters `params`.
Returns `(notes, polys, model)` where `notes` is the list of notes in the piece
and `polys` is the list of matching polygrams sorted by their match score (best first).
`model` is a tuple of the form `(features, keys, modelform)`
that can be used to fit the heuristic parameters.
"""
function matchpiece(pieceid, schemata;
                    params=[1, -1//12, -2], corpus=getcorpus(),
                    ignorecache=false, overwritecache=false, cachedir=projectdir("data", "polys"))
    # load piece
    notes = getpiece(pieceid, :notes_wholes, corpus);
    barlen = piecebarlen(pieceid, corpus);
    timesigs = getpiece(pieceid, :timesigs, corpus);

    # set up heuristics
    beatfactor = denominator(content(timesigs[1]))
    featurefs = [
        poly -> beatfactor * Polygrams.totalduration(poly), # duration
        Polygrams.voicedist,                                # voice dist
        poly -> beatfactor * Polygrams.instageskip(poly)    # stage skip
        #, poly -> Polygrams.polymweight(poly, timesigs) # metric weight
    ]
    featurekeys = [:dur, :vdist, :skip]
    # modelform = @formula(label ~ dur + vdist + skip)

    # cache filenames
    escid = replace(pieceid, r"[\\/]" => s"_")
    cachefn(schemaid) = joinpath(cachedir, "$(escid)_$(schemaid).jld2")
    if !isdir(cachedir)
        mkpath(cachedir, mode=0o770) # ensure cachedir path exists
    end
    
    # retrieve matches
    missingschemas = valtype(schemata)[]
    polys = nothing
    for (schemaid, schema) in schemata
        fn = cachefn(schemaid)
        if isfile(fn) && !ignorecache
            @info "loading $escid, $schemaid from cache"
            loaded = loadpolys(fn)
            if polys == nothing
                polys = loaded
            else
                append!(polys, loaded)
            end
        else
            @info "need to recompute polys for $escid, $schemaid"
            push!(missingschemas, schema)
        end
    end

    # generate missing polygrams
    if !isempty(missingschemas)
        @info "recomputing schemata" missingschemas
        polyitr = Polygrams.schemamatches(notes, missingschemas, barlen, barlen)
        missingpolys = collect(polyitr)
        if polys == nothing
            polys = missingpolys
        else
            append!(polys, missingpolys)
        end
    
        # save missing polygrams
        @info "grouping recomputed polys"
        groupedpolys = grouppolys(missingpolys, Dict(proto => id for (id, proto) in schemata))
        for (schemaid, polys) in groupedpolys
            fn = cachefn(schemaid)
            if !isfile(fn) || overwritecache
                @info "saving new cache entry for $(escid), $(schemaid)"
                savepolys(fn, polys)
            end
        end
    end

    # sort all polygrams using heuristics
    sorted = Polygrams.sortbyheuristics(polys, featurefs, params)

    # return everything
    (notes, sorted, (featurefs, featurekeys)) #, modelform))
end
