module SchemaTrie

export mktrie, update!

import Base: get, getindex, setindex!, convert, collect, merge!

mutable struct Trie{T, V}
    children :: Dict{T, Trie{T, V}}
    value :: Nullable{V}
end

mktrie(T::Type, V::Type) = Trie(Dict{T,Trie{T,V}}(), Nullable{V}())

function get(trie::Trie{T,V}, key, def::V) where {T, V}
    for k in key
        if haskey(trie.children, k)
            trie = trie.children[k]
        else
            default
        end
    end
    get(trie.value, default)
end

function getindex(trie::Trie{T,V}, key) where {T,V}
    for k in key
        if haskey(trie.children, k)
            trie = trie.children[k]
        else
            throw(KeyError("key not found: $key"))
        end
    end
    if isnull(trie.value)
        throw(KeyError("key not found: $key"))
    else
        get(trie.value)
    end
end

function setindex!(trie::Trie{T,V}, value::V, key) where {T, V}
    for k in key
        if !haskey(trie.children, k)
            trie.children[k] = mktrie(T, V)
        end
        trie = trie.children[k]
    end
    trie.value = Nullable{V}(value)
end

function update!(f, trie::Trie{T,V}, key, default::V) where {T, V}
    for k in key
        if !haskey(trie.children, k)
            trie.children[k] = mktrie(T, V)
        end
        trie = trie.children[k]
    end
    v = f(get(trie.value, default))
    trie.value = Nullable{V}(v)
    v
end

merge!(f, t1::Trie{T,V1}, t2::Trie{T,V2}) =

function dictify!(trie, dict, pfx)
    if !isnull(trie.value)
        dict[pfx] = get(trie.value)
    end
    for (k, child) in trie.children
        dictify!(child, dict, [pfx..., k])
    end
    dict
end

convert(::Type{Dict}, trie::Trie{T,V}) where {T,V} =
    dictify!(trie, Dict{Vector{T}, V}(), [])

function collecttrie!(trie, out, pfx)
    if !isnull(trie.value)
        push!(out, pfx=>get(trie.value))
    end
    for (k, child) in trie.children
        collecttrie!(child, out, [pfx..., k])
    end
    out
end

collect(trie::Trie{T,V}) where {T,V} =
    collecttrie!(trie, Vector{Pair{Vector{T}, V}}(), [])

end # module
