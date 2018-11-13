import Pkg; Pkg.activate("..");

include(joinpath(@__DIR__, "..", "src", "polygrams.jl"))

using DigitalMusicology

corpora = Dict(
    "mozart-piano-sonatas" => (
        kerncrp(joinpath(@__DIR__, "..", "data", "corpora", "mozart-piano-sonatas")),
        "Mozart Piano Sonatas"
    ),
    "beethoven-piano-sonatas" => (
        kerncrp(joinpath(@__DIR__, "..", "data", "corpora", "beethoven-piano-sonatas")),
        "Beethoven Piano Sonatas"
    ),
    "beethoven-string-quartets" => (
        kerncrp(joinpath(@__DIR__, "..", "data", "corpora", "beethoven-string-quartets")),
        "Beethoven String Quartets"
    ),
    "chopin-mazurkas" => (
        kerncrp(joinpath(@__DIR__, "..", "data", "corpora", "chopin-mazurkas")),
        "Chopin Mazurkas"
    )
)

Polygrams.polyserve(Polygrams.schemaapp(corpora), port=61000)
