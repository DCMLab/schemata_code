module Automata

using IterTools: subsets
import Base: show

export compile, initial, nextstate, isaccepting
export accepts, preaccepts
export DFA, NFA, words2nfa
export showdot, todot, savedot

# Automaton Interface
# ===================

"""
    compile(auto)

Returns a compiled or somehow optimized but equivalent version of the automaton.
"""
function compile end

"""
    initial(auto, [input])

Returns the automatons inital state.
For automata that represent a complex memory based on the input
(like a touring machine, where the input is written on the band),
the input must be supplied.
"""
function initial end

"""
    nextstate(auto, state, [input])

Returns the next state of the automaton from the current state.
For automata that traverse the input once, the next symbol is provided here
For decision automata, the returned value is only the next state.
For transducers, the returned value is a tuple (next, output).
The result is `nothing` if there is no legal transition
from that state (and the given input, if applicable).
This case is equivalent to failure (or rejection),
so rejection states are modeled implicitely as `nothing`.
"""
function nextstate end

"""
    isaccepting(auto, state)

Returns `true`, iff the given state is an accepting (or final) state.
"""
function isaccepting end

"""
    accepts(auto, input)

Runs an automaton `auto` on a sequence of items `input`.
Returns `true`, iff the automaton accepts the input.
"""
function accepts end

"""
    preaccepts(auto, input)

Runs an automaton `auto` on a sequence of items `input`.
Returns `true`, iff the automaton does not fail on the input,
even if the last state is not an accepting (or final) state.
"""
function preaccepts end

showdot(io::IO, automaton) = show(io, MIME("text/x-dot"), automaton)

todot(automaton) =
    let io = IOBuffer()
        showdot(io, automaton)
        String(take!(io))
    end

savedot(automaton, fn) =
    open(fn, "w") do fs
        showdot(fs, automaton)
    end

# Finite State Automata
# =====================

# DFA
# ---

struct DFA{S,A}
    init   :: S
    finals :: Set{S}
    trans  :: Dict{Tuple{S,A},S}
end

# minimizes the automaton
function compile(dfa::DFA{S,A}) where {S,A}
    alphabet = Set(symb for ((_,symb), to) in dfa.trans)
    states = Set(to for (_, to) in dfa.trans)
    push!(states, dfa.init)
    partition = Set([
        dfa.finals,
        setdiff(states, dfa.finals)
    ])
    agenda = Set([dfa.finals])

    while !isempty(agenda)
        target = pop!(agenda)
        for symb in alphabet
            newpart = Set{Set{S}}()
            for sub in partition
                leads = filter(sub) do st
                    haskey(dfa.trans, (st, symb)) && dfa.trans[(st, symb)] ∈ target
                end
                leadsnot = setdiff(sub, leads)
                if !isempty(leads) && !isempty(leadsnot)
                    push!(newpart, leads)
                    push!(newpart, leadsnot)
                    if sub ∈ agenda
                        delete!(agenda, sub)
                        push!(agenda, leads)
                        push!(agenda, leadsnot)
                    else
                        push!(agenda, length(leads) <= length(leadsnot) ? leads : leadsnot)
                    end
                else
                    push!(newpart, sub)
                end
            end
            partition = newpart
        end
    end
    
    statemap = Dict(s => first(first(filter(sub -> s ∈ sub, partition))) for s in states)
    equivify(state) = statemap[state]
    
    init = equivify(dfa.init)
    finals = Set(equivify(f) for f in dfa.finals)
    trans = Dict((equivify(from), symb) => equivify(to)
                 for ((from, symb), to) in dfa.trans)
    DFA{S,A}(init, finals, trans)
end

initial(dfa::DFA{S,A}) where {S,A} = dfa.init

nextstate(dfa::DFA{S,A}, state::S, input::A) where {S,A} =
    if haskey(dfa.trans, (state, input))
        dfa.trans[(state, input)]
    else
        nothing
    end

isaccepting(dfa::DFA{S,A}, state::S) where {S,A} =
    state ∈ dfa.finals

function partialmatch(dfa::DFA{S,A}, input) where {S,A}
    state = initial(dfa)
    #newst = nothing
    for i in input
        #newst = nextstate(dfa, state, i)
        state = nextstate(dfa, state, i)
        if state == nothing
            return nothing
        # else
        #     state = newst
        end
    end
    state
end

preaccepts(dfa::DFA, input) = (partialmatch(dfa, input)) != nothing

function accepts(dfa::DFA, input)
    state = partialmatch(dfa, input)
    if state == nothing
        return false
    else
        isaccepting(dfa, state)
    end
end

function show(io::IO, ::MIME"text/x-dot", dfa::DFA{S,A}) where {S,A}
    counter = 0
    mapping = Dict{S,Int}()
    function intify(state)
        if haskey(mapping, state)
            mapping[state]
        else
            mapping[state] = counter
            counter += 1
            counter - 1
        end
    end

    write(io, "digraph {\n")
    write(io, "  rankdir=LR;")

    initn = string("q", intify(dfa.init))
    write(io, "  entry[shape=point];\n")
    write(io, "  $(initn)[shape=circle];\n")
    write(io, "  entry -> $(initn);\n")

    for final in dfa.finals
        write(io, "  q$(intify(final))[shape=doublecircle];\n")
    end

    write(io, "  node [shape = circle];\n")
    for (from, to) in dfa.trans
        fn = string("q", intify(from[1]))
        tn = string("q", intify(to))
        write(io, """  $(fn) -> $(tn) [label="$(string(from[2]))"];""", "\n")
    end

    write(io, "}");
end

# NFA
# ---

struct NFA{S,A}
    init   :: S
    finals :: Set{S}
    trans  :: Dict{Tuple{S,A}, Set{S}}
end

function compile(nfa::NFA{S,A}) where {S,A}
    counter = 0
    mapping = Dict{Set{S},Int}()
    function intify(set)
        if haskey(mapping, set)
            mapping[set]
        else
            mapping[set] = counter
            counter += 1
            counter - 1
        end
    end

    init   = intify(Set(nfa.init))
    finals = Set{Int}() #map(intify ∘ Set, subsets(nfa.finals))

    trans  = Dict{Tuple{Int,A}, Int}()
    agenda = [Set(nfa.init)]
    seen = Set{Int}(intify(agenda[1]))

    while !isempty(agenda)
        st = pop!(agenda)
        sti = intify(st)
        transfromst = filter(edge -> edge.first[1] ∈ st, nfa.trans)
        symbols = Set(from[2] for (from, to) in transfromst)
        for symb in symbols
            nextst = reduce(∪, [to for (from, to) in transfromst if from[2] == symb])
            nextsti = intify(nextst)
            trans[(sti, symb)] = nextsti
            if any(subst -> subst ∈ nfa.finals, nextst)
                push!(finals, nextsti)
            end
            if nextsti ∉ seen
                push!(seen, nextsti)
                push!(agenda, nextst)
            end
        end
    end
    
    compile(DFA(init, finals, trans))
end

# create a trivial nfa for a list of words:
# start from a common start state and add a new linear path
# with new states for each word, collecting the final states.
function words2nfa(words::T) where T
    A = eltype(eltype(T))
    init = 0
    finals = Set{Int}()
    counter = 0
    trans = Dict{Tuple{Int, A}, Set{Int}}()
    for word in words
        counter += 1
        push!(get!(trans, (init, word[1]), Set{Int}()), counter)
        for w in word[2:end]
            counter += 1
            trans[(counter-1, w)] = Set(counter)
        end
        push!(finals, counter)
    end
    NFA(init, finals, trans)
end

function show(io::IO, ::MIME"text/x-dot", nfa::NFA{S,A}) where {S,A}
    counter = 0
    mapping = Dict{S,Int}()
    function intify(state)
        if haskey(mapping, state)
            mapping[state]
        else
            mapping[state] = counter
            counter += 1
            counter - 1
        end
    end

    write(io, "digraph {\n")
    write(io, "  rankdir=LR;")

    initn = string("q", intify(nfa.init))
    write(io, "  entry[shape=point];\n")
    write(io, "  $(initn)[shape=circle];\n")
    write(io, "  entry -> $(initn);\n")

    for final in nfa.finals
        write(io, "  q$(intify(final))[shape=doublecircle];\n")
    end

    write(io, "  node [shape = circle];\n")
    for (from, to) in nfa.trans
        fn = string("q", intify(from[1]))
        for t in to
            tn = string("q", intify(t))
            write(io, """  $(fn) -> $(tn) [label="$(string(from[2]))"];""", "\n")
        end
    end

    write(io, "}");
end

end # module
