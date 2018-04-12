include("fsa.jl")

testdfa = Automata.DFA(0, Set([3]), Dict((0, 'a') => 1, (1, 'b') => 2, (2, 'c') => 3))

Automata.preaccepts(testdfa, "a")

testnfa = Automata.words2nfa(["abcd", "aecd"])

testtrans = Automata.compile(testnfa)
