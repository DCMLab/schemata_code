using Interact
using Base.Iterators: partition
using DataStructures
using Mux

jsdep(dep) = joinpath(@__DIR__, "..", "deps", dep)

"""
    level(items)
    level(items...)

Takes a list of dom elements and wraps them in bulma.css' level elements (left-aligned).
"""
level(items) = dom"div.level"(dom"div.level-left"(map(dom"div.level-item", items)...))
level(items...) = level(items)

"""
    ratingswdg(ratings)

Creates a widget plotting the extents of polygrams on the x axis
and their scores on the y axis.
`ratings` are expected to be named triples `(onset,offset,score)`.
"""
function ratingswdg(ratings; highlights=[])
    # "https://d3js.org/d3.v4.min.js"
    scp = Scope(imports=[jsdep("d3.v4.min.js"), jsdep("ratings.js")])
    id = string("ratings", rand(UInt))

    oratings = Observable(ratings)
    setobservable!(scp, "ratings", oratings)

    ohighlights = Observable(highlights)
    setobservable!(scp, "highlights", ohighlights)

    onimport(scp, js"""function(d3) {
        ratings($id, d3, $oratings.val, $ohighlights.val);
    }""")

    onjs(scp["ratings"], js"rs => document.getElementById($id).updateRatings(rs)")
    onjs(scp["highlights"], js"hls => document.getElementById($id).updateHighlights(hls)")

    lay(w) = node(:div,
                  scope(w),
                  id=id,
                  style=Dict("width" => "90%", "background" => "white"))
    wdg = Widget([:ratings => oratings, :highlights => ohighlights];
                 scope=scp, layout=lay)
end

"""
    polydensities(ranges)

Takes a list of tuples `(onset, offset)` indicating the range of a polygram.
Returns a list of named tuples `(onset, offset, count)`
indicating how many polygrams occur in any span of time.
Timespans with zero polygrams are implicit and not returned.
"""
function polydensities(ranges; notes=nothing)
    # find breaks
    breaks = Set()
    if notes == nothing
        for r in ranges
            push!(breaks, r[1])
            push!(breaks, r[2])
        end
    else
        for n in notes
            push!(breaks, onset(n))
            push!(breaks, offset(n))
        end
    end
    breaks = sort!(collect(breaks))
    bins = zeros(Int, length(breaks)-1)

    for range in ranges
        for i in 1:length(bins)
            if breaks[i] >= range[2]
                break;
            elseif breaks[i] >= range[1]
                bins[i] += 1
            end
        end
    end

    map(bins, breaks[1:end-1], breaks[2:end]) do count, on, off
        (onset=on, offset=off, value=count)
    end
end

testdata = [
    (onset=0, offset=1, value=10),
    (onset=1, offset=3, value=7 ),
    (onset=3, offset=3.5, value=12)
];

"""
    polydensitywdg(hist)

Takes a list of named tuples `(onset, offset, values)`
which are interpreted as a histogram with uneven bins.
Returns an Interact widget that displays this histogram.
"""
function polydensitywdg(hist)
    scp = Scope(imports=[jsdep("vega@4.2.0.js"),
                         jsdep("vega-lite@3.0.0-rc6.js"),
                         #jsdep("vega-embed@3.19.2.js"),
                         jsdep("density.js")])
    id = string("density", rand(UInt))

    onimport(scp, js"""function(vega, vl) {
        density($id, vega, vl, $hist);
    }""")

    lay(w) = node(:div, scope(w), id=id, style=Dict("width" => "90%"))
    wdg = Widget([]; scope=scp, layout=lay)
end

function matchinteractive(notes, sortedpolys)
    best = Observable(bestmatches(!polyssharetime, sortedpolys))
    alternatives = nothing
    current = Observable(1)
    hits = Observable(trues(length(best[])))
    matches = map(getindex, best, hits)
    
    # matcher elements

    altslider = Observable(slider(1:10))
    pr = pianorollwdg(notes)
    
    function refresh()
        alternatives = findcompetitors(polyssharetime, best[][current[]], sortedpolys)
        sl = slider(1:length(alternatives), value=1)
        on(sl) do alt
            best[][current[]] = alternatives[alt]
            best[] = best[]
            pr[:highlights][] = [vcat(alternatives[alt]...)]
        end
        altslider[] = sl
        altslider[][] = findfirst(alt -> alt == best[][current[]], alternatives)
        ishit[] = hits[][current[]]
    end

    next = button("Next")
    on(next) do _
        current[] = min(length(best[]), current[]+1)
        refresh()
    end
    prev = button("Previous")
    on(prev) do _
        current[] = max(1, current[]-1)
        refresh()
    end
    ishit = toggle(label="It's a match!", true)
    on(ishit) do ih
        hits[][current[]] = ih
        hits[] = hits[]
    end

    refresh()

    # ratings widget
    
    wdg = Widget([:matches => matches], output=matches)
    controls = ["Match No.", prev, current, next, "Alternative:", altslider, ishit]
    ctrldom = level(controls) # dom"div.level"(dom"div.level-left"(map(dom"div.level-item", controls)...))
    @layout! wdg vbox(pr, ctrldom)
                                         
end

function markschemas(notes, schemas)
    marked = Observable(SortedSet(Base.By(x->onset(x[1][1]))))
    highlighted = Observable([])

    pr = pianorollwdg(notes, allowselect=true)

    on(pr[:selected]) do selected
        for schema in schemas
            if length(selected) == sum(length, schema)
                sel = sort(selected, by=onset)
                poly = collect(partition(sel, length(schema[1])))
                if schemarep(poly) == schema
                    marked[] = push!(marked[], poly)
                    highlighted[] = poly
                    clear!(pr)
                end
            end
        end
    end

    Observables.@map! pr[:highlights] [polynotes(&highlighted)]

    function mkrow(poly, i, highl)
        onset, offset = polyrange(poly)
        shw = button("show")
        on(shw) do _
            highlighted[] = poly
        end

        if poly == highl
            del = button("delete")
            on(del) do _
                highlighted[] = []
                pop!(marked[], poly)
                marked[] = marked[]
            end
            level(shw, "$i. from $onset to $offset", del)
        else
            level(shw, "$i. from $onset to $offset")
        end
    end
    
    table = map(marked, highlighted) do ms, hl
        rows = map(ms, 1:length(ms)) do m, i
            mkrow(m, i, hl)
        end
        vbox(rows)
    end
    
    wdg = Widget([:marked => marked]; output=marked)
    @layout! wdg hbox(dom"div"(pr, style=Dict("width"=>"75%")),
                      dom"div"(table, style=Dict("width"=>"25%")))
end

function annotateview(pieceid, schemaids, weights;
                        lexicon=projectdir("data", "lexicon_flat.json"),
                        annotdir=projectdir("data", "autoannot"),
                        cachedir=projectdir("data", "polys"),
                        corpus=getcorpus())
    lex = loadlexicon(lexicon)
    schemata = Dict(sid => lex[sid] for sid in schemaids)
    schemadict = Dict(lex[sid] => sid for sid in schemaids)
    barlen = piecebarlen(pieceid, corpus)
    notes, sorted, (features, fkeys) = matchpiece(pieceid, schemata, params=weights,
                                                  corpus=corpus, cachedir=cachedir)
    
    match = matchinteractive(notes, sorted)
    mark = markschemas(notes, values(schemata))

    function mkhls(matched, marked)
        mtd = vcat(map(polynotes,matched)...)
        mkd = vcat(map(polynotes,marked)...)
        [mtd, mkd]
    end

    overview = pianorollwdg(notes, zoomtohl=false, highlights=mkhls(match[], mark[]))
    map!(mkhls, overview[:highlights], match, mark)

    savebtn = button("Save")
    on(savebtn) do _
        allmatches = collect(mark[])
        union!(allmatches, match[])
        annots = grouppolys(allmatches, schemadict)
        # for poly in allmatches
        #     sid = schemadict[schemarep(poly)]
        #     if haskey(annots, sid)
        #         push!(annots[sid], poly)
        #     else
        #         annots[sid] = [poly]
        #     end
        # end

        for (sid, polys) in annots
            saveannots(pieceid, sid, polys, annotdir)
        end
    end
    
    dom"div.container"(dom"h1.title.is-1"("Annotating $pieceid: $(join(schemaids, ", "))"),
                       "Bar length: $barlen",
                       dom"h2.title.is-2"("Automatic Matcher"), match,
                       dom"h2.title.is-2"("Manual Annotations"), mark,
                       dom"h2.title.is-2"("Overview"), overview,
                       savebtn)
end

function exploreview(pieceid, schemaids, weights;
                     lexicon=projectdir("data", "lexicon_flat.json"),
                     cachedir=projectdir("data", "polys"),
                     corpus=getcorpus())
    lex = loadlexicon(lexicon)
    schemata = Dict(sid => lex[sid] for sid in schemaids)
    # schemadict = Dict(lex[sid] => sid for sid in schemaids)
    barlen = piecebarlen(pieceid, corpus)
    notes, sorted, (features, fkeys) = matchpiece(pieceid, schemata, params=weights,
                                                  corpus=corpus, cachedir=cachedir)
    
    match = matchinteractive(notes, sorted)

    function mkrating(poly)
        ext = polyrange(poly)
        (onset=ext[1], offset=ext[2], score=rate(poly, features, weights))
    end
    ratings = mkrating.(sorted)
    wratings = ratingswdg(ratings)

    Observables.@map! wratings[:highlights] [mkrating.(&match)]
    
    dhist = polydensities(polyrange.(sorted), notes=notes)
    wdens = polydensitywdg(dhist)
    
    dom"div.container"(dom"h1.title.is-1"("Exploring $pieceid: $(join(schemaids, ", "))"),
                       "Bar length: $barlen",
                       dom"h2.title.is-2"("Automatic Matcher"), match,
                       dom"h2.title.is-2"("Ratings"), wratings,
                       dom"h2.title.is-2"("Density"), wdens)
end

"""
    overview(corpora)

Takes a dictionary `corpora` of the form  `id => (corpus, description)`,
where `corpus` is a `Corpus` object, and `id` and `description` are strings.
"""
function overview(corpora; lexicon=projectdir("data", "lexicon_flat.json"))
    rdcorpus = radiobuttons(OrderedDict(desc[2] => id for (id, desc) in corpora))
    lex = loadlexicon(lexicon)
    ckschemata = checkboxes(OrderedDict(schema => schema for schema in keys(lex)),
                            value=[first(keys(lex))])

    piecelinks = map(rdcorpus, ckschemata) do corpus, schemata
        OrderedDict(id => "/$corpus/$id/$(join(schemata, ','))/" for id in allpieces(corpora[corpus][1]))
    end

    explorelinks = map(piecelinks) do plinks
        links = [dom"a"(id, href="/explore"*href) for (id, href) in plinks]
        vbox(dom"h2.title.is-2"("Explore"), links...)
    end

    annotatelinks = map(piecelinks) do plinks
        links = [dom"a"(id, href="/annotate"*href) for (id, href) in plinks]
        vbox(dom"h2.title.is-2"("Annotate"), links...)
    end

    
    dom"div.container"(
        dom"h1.title.is-1"("Overview"),
        dom"p"("Select the corpus, piece, and schema that you want to look at:"),
        hbox(vbox(rdcorpus, ckschemata), explorelinks, annotatelinks)
    )
end

# type App = Request -> Response
# type Middle = App -> Request -> Response
# mux :: Middle -> App -> App
# stack :: [Middle] -> Middle
# branch :: (Request -> Bool) -> App -> Middle

function logreq(app, req)
    println(req)
    app(req)
end

function exploreapp(corpora)
    function (req)
        piece = req[:params][:piece]
        schemas = split(req[:params][:schemas], ',')
        corpus = req[:params][:corpus]
        exploreview(piece, schemas, [1, -1//12, -2];
                    cachedir=projectdir("data", "polys", corpus),
                    corpus=corpora[corpus][1])
    end
end

function annotateapp(corpora)
    function (req)
        piece = req[:params][:piece]
        schemas = split(req[:params][:schemas], ',')
        corpus = req[:params][:corpus]
        annotateview(piece, schemas, [1, -1//12, -2];
                     cachedir=projectdir("data", "polys", corpus),
                     corpus=corpora[corpus][1])
    end
end

function schemaapp(corpora)
    stack(
        page("/", respond(overview(corpora))),
        page("/explore/:corpus/:piece/:schemas/", exploreapp(corpora)),
        page("/annotate/:corpus/:piece/:schemas/", annotateapp(corpora))
    )
    #stack(page(respond(overview(corpora))))
end

function polyserve(app; host=Mux.ip"127.0.0.1", port=8000)
    http = Mux.App(Mux.mux(
        Mux.defaults,
        app,
        Mux.notfound()
    ))

    websock = Mux.App(Mux.mux(
        Mux.wdefaults,
        Mux.route("/webio-socket", WebIO.create_socket),
        Mux.wclose,
        Mux.notfound()
    ))

    Mux.serve(http, websock, host, port)
end

# function annotate(;corpus=getcorpus(), lexicon="data/lexicon_flat.json")
#     ids = dropdown(merge!(OrderedDict("Select Piece..." => ""),
#                           OrderedDict(id => id for id in allpieces(corpus))),
#                   label="Piece:")
#     schemas = dropdown(merge(OrderedDict("Select Schema" => Vector{MidiPitch}[]),
#                              sort(loadlexicon(lexicon))),
#                        label="Schema:")
#     load = button("Load")

#     matcherwdg = map(load) do _
#         if !(ids[] == "" || schemas[] == [])
#             notes, polys, _ = matchpiece(ids[], [schemas[]])
#             matchinteractive(notes, polys)
#         end
#     end
    
#     vbox(hbox(ids, schemas, load), matcherwdg)
# end
