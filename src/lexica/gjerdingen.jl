module Gjerdingen

using DigitalMusicology

include("common.jl")

export fonte

@macroexpand @schema fonte "2v" [
    [7, 1],
    [5, 2],
    [5, -1],
    [4, 0]
]

end # module Gjerdingen
