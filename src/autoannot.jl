using Interact
using Base.Iterators: partition
using DataStructures
using Mux

jsdep(dep) = joinpath(@__DIR__, "..", "deps", dep)

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

function annotationview(pieceid, schemaids, weights;
                        lexicon="../data/lexicon_flat.json", annotdir="../data/autoannot/")
    lex = loadlexicon(lexicon)
    schemata = map(s -> lex[s], schemaids)
    schemadict = Dict(lex[sid] => sid for sid in schemaids)
    notes, sorted, (features, fkeys) = matchpiece(pieceid, schemata, params=weights)
    
    match = matchinteractive(notes, sorted)
    mark = markschemas(notes, schemata)

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
        annots = Dict()
        for poly in allmatches
            sid = schemadict[schemarep(poly)]
            if haskey(annots, sid)
                push!(annots[sid], poly)
            else
                annots[sid] = [poly]
            end
        end

        for (sid, polys) in annots
            saveannots(pieceid, sid, polys, annotdir)
        end
    end
    
    vbox(dom"h2.title.is-2"("Automatic Matcher"), match,
         dom"h2.title.is-2"("Manual Annotations"), mark,
         dom"h2.title.is-2"("Overview"), overview,
         savebtn)
end

function annotate(;corpus=getcorpus(), lexicon="data/lexicon_flat.json")
    ids = dropdown(merge!(OrderedDict("Select Piece..." => ""),
                          OrderedDict(id => id for id in allpieces(corpus))),
                  label="Piece:")
    schemas = dropdown(merge(OrderedDict("Select Schema" => Vector{MidiPitch}[]),
                             sort(loadlexicon(lexicon))),
                       label="Schema:")
    load = button("Load")

    matcherwdg = map(load) do _
        if !(ids[] == "" || schemas[] == [])
            notes, polys, _ = matchpiece(ids[], [schemas[]])
            matchinteractive(notes, polys)
        end
    end
    
    vbox(hbox(ids, schemas, load), matcherwdg)
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
