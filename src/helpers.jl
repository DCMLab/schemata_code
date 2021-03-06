import FunctionalCollections: append
import Base: foldr

# appending lists
append(l1::EmptyList, l2) = l2
append(l1, l2::EmptyList) = l1
append(l1::plist{T}, l2::plist{T}) where T =
    head(l1) .. append(tail(l1), l2)

foldr(op, v0, lst::EmptyList) = v0
foldr(op, v0, lst::plist{T}) where T =
    op(head(lst), foldr(op, v0, tail(lst)))
foldr(op, lst::plist{T}) where T =
    if length(lst) <= 1
        head(lst)
    else
        op(head(lst), foldr(op, tail(lst)))
    end

projectdir(args...) = joinpath(@__DIR__, "..", args...)

function groupby(f, xs)
    # guess type of keys
    ktype = typeof(f(first(xs)))
    
    out = Dict{ktype,Vector{eltype(xs)}}()
    for x in xs
        key = f(x)
        if haskey(out, key)
            push!(out[key], x)
        else
            out[key] = [x]
        end
    end
    out
end
