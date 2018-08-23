include("fsa.jl")

testdfa = Automata.DFA(0, Set([3]), Dict((0, 'a') => 1, (1, 'b') => 2, (2, 'c') => 3))

Automata.preaccepts(testdfa, "a")

testnfa = Automata.words2nfa(["abcd", "aefd"])

testtrans = Automata.compile(testnfa)

###

using Revise

using DigitalMusicology
DigitalMusicology.usekern("/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/");

# using Plots; plotly()
using MusicologyPlots.VegaPlots

include("src/polygrams.jl")

prinner = @midi [[0, 4], [11, 2], [9, 0], [7, 11]]
fonte = @midi [[0, 6], [1, 4], [10, 4], [11, 3]]

pieceid = "sonata15-1";
notes = getpiece(pieceid, :notes_wholes);
barlen = Polygrams.piecebarlen(pieceid);

schemata = Polygrams.schemamatches(notes, [prinner], barlen, barlen);

@time polys = collect(schemata);

combweight(poly) = exp(Polygrams.totalduration(poly) - 0.1Polygrams.voicedist(poly))
wpolys = Polygrams.sortbyweight(polys, combweight);

#best = Polygrams.bestmatches(!Polygrams.polyssharenotes, wpolys);
best = Polygrams.bestmatches(!Polygrams.polyssharetime, wpolys);

plotpolygrams(notes, map(first, wpolys[1:10]))

@time classes = Polygrams.transitiveoverlap(polys);

#Polygrams.savepolys("test.jld", polys)

#loaded = Polygrams.loadpolys("test.jld")
