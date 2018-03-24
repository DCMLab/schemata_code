module SchemaTrie

export mktrie, update!

import Base: get, getindex, setindex!, convert, collect, merge!, length

abstract type Trie{T,V} end

struct Leaf{T,V} <: Trie{T,V}
    value :: V
end

struct SubTrie{T, V} <: Trie{T,V}
    children :: Dict{T, Trie{T, V}}
end

mktrie(T::Type, V::Type) = SubTrie(Dict{T,Trie{T,V}}())

isleaf(t) = false
isleaf(t::Leaf) = true

length(t::SubTrie) = sum(map(length, values(t.children)))
length(t::Leaf) = 1

function get(trie::Trie{T,V}, key, def::V) where {T, V}
    for k in key
        if haskey(trie.children, k)
            trie = trie.children[k]
        else
            default
        end
    end
    if isleaf(trie)
        trie.value
    else
        default
    end
end

function getindex(trie::Trie{T,V}, key) where {T,V}
    for k in key
        if haskey(trie.children, k)
            trie = trie.children[k]
        else
            throw(KeyError("key not found: $key"))
        end
    end
    if isleaf(trie)
        trie.value
    else
        throw(KeyError("key not found: $key"))
    end
end

function setindex!(trie::Trie{T,V}, value::V, key) where {T, V}
    for k in key[1:end-1]
        if !haskey(trie.children, k)
            trie.children[k] = mktrie(T, V)
        end
        trie = trie.children[k]
    end
    trie.children[key[end]] = Leaf{T,V}(value)
end

function update!(f, trie::Trie{T,V}, key, default::V) where {T, V}
    for k in key[1:end-1]
        if !haskey(trie.children, k)
            trie.children[k] = mktrie(T, V)
        end
        trie = trie.children[k]
    end
    ke = key[end]
    if haskey(trie.children, ke) && isleaf(trie.children[ke])
        v = f(trie.children[ke].value)
    else
        v = f(default)
    end
    trie.children[ke] = Leaf{T,V}(v)
    v
end

merge!(combine, t1::SubTrie{T,V}, t2::SubTrie{T,V}) where {T,V} =
    SubTrie{T,V}(merge!((s1, s2) -> merge!(combine, s1, s2), t1.children, t2.children))
merge!(combine, t1::Leaf{T,V}, t2::Leaf{T,V}) where {T,V} =
    Leaf{T,V}(combine(t1.value, t2.value))
merge!(combine, t1::SubTrie{T,V}, ::Leaf{T,V}) where {T,V} = t1
merge!(combine, ::Leaf{T,V}, t2::SubTrie{T,V}) where {T,V} = t2

function dictify!(trie::Leaf, dict, pfx)
    dict[pfx] = trie.value
    dict
end

function dictify!(trie::SubTrie, dict, pfx)
    for (k, child) in trie.children
        dictify!(child, dict, [pfx..., k])
    end
    dict
end

convert(::Type{Dict}, trie::Trie{T,V}) where {T,V} =
    dictify!(trie, Dict{Vector{T}, V}(), [])

function collecttrie!(trie::Leaf, out, pfx)
    push!(out, pfx=>trie.value)
    out
end

function collecttrie!(trie::SubTrie, out, pfx)
    for (k, child) in trie.children
        collecttrie!(child, out, [pfx..., k])
    end
    out
end

collect(trie::Trie{T,V}) where {T,V} =
    collecttrie!(trie, Vector{Pair{Vector{T}, V}}(), [])

end # module
