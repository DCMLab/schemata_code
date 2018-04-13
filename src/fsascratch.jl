include("fsa.jl")

testdfa = Automata.DFA(0, Set([3]), Dict((0, 'a') => 1, (1, 'b') => 2, (2, 'c') => 3))

Automata.preaccepts(testdfa, "a")

testnfa = Automata.words2nfa(["abcd", "aefd"])

testtrans = Automata.compile(testnfa)

###

using DigitalMusicology
DigitalMusicology.usekern("/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/");

include("polygrams.jl")

prinner = @midi [[0, 4], [11, 2], [9, 0], [7, 11]]
fonte = @midi [[0, 6], [1, 4], [10, 4], [11, 3]]

pieceid = "sonata03-2"
notes = getpiece(pieceid, :notes_wholes)
barlen = Polygrams.piecebarlen(pieceid)

schemata = Polygrams.schemamatches(notes, [prinner, fonte], barlen, barlen)
