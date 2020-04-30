# common.jl

"""
    schemamatrix(notes)

Transforms nested vectors into a matrix.
"""
function schemamatrix(notes::Vector{Vector{N}}) where N
    s = length(notes)
    v = length(notes[1])
    m = Array{eltype(notes[1]),2}(undef, s, v)

    for i in 1:s
        for j in 1:v
            m[i,j] = notes[i][j]
        end
    end
    
    m
end

"""
    schemarep(intervals; sortstages=true, toic=true)

Turns a matrix of intervals into the interval representation for schemata.
The input and output matrix represents stages as rows and voices as columns (lowest to highest).
Every interval is expressed relative to the element at [1,1].

If `sortstages` is `true`, then the elements in each stage (row) will be sorted before processing.
Use this if the elements come from the skipgram matcher and are arranged in temporal order.
Don't use this if the intervals come from the schema lexicon,
where they already represent interval classes.

If `toic` is true, intervals are converted to interval classes after processing.
"""
function schemarep(ints::Array{I,2}; toic=true, sortstages=true) where {I<:Interval}
    if sortstages
        ints = sort(ints, dims=2) # sort pitch groups (lowest to highest)
    end
    ref = ints[1,1]    # transpose -> reference pitch = 0, pc(...):
    map(ints) do int
        if toic
            ic(int - ref)
        else
            int - ref
        end
    end
end

"""
    schemarep(notes; ...)

Turns nested vectors of notes into an interval matrix.
Each inner vector represents a stage.
Takes the same keyword arguments as `schemarep(intervals; ...)`.
"""
function schemarep(notes::Vector{Vector{N}}; kwargs...) where {N<:Note}
    m = schemamatrix(notes)
    
    m = map(tointerval ∘ pitch, m)
    schemarep(m;kwargs...) # extract pitches from timed notes
end
