import Pkg; Pkg.activate("..");

include(joinpath(@__DIR__, "..", "src", "polygrams.jl"))

using DigitalMusicology

corpora = Dict(
    "mozart-piano-sonatas" => (
        kerncrp("/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/"),
        "Mozart Piano Sonatas"
    ),
    "beethoven-string-quartets" => (
        kerncrp("/home/chfin/Uni/phd/data/csapp/beethoven-string-quartets/"),
        "Beethoven String Quartets"
    )
)

Polygrams.polyserve(Polygrams.schemaapp(corpora))
