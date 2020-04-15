# classify.jl

using DigitalMusicology

include("io.jl")



function loadpiecedata(corpusdir, pieceid, schemaid)
    usedir(joinpath(corpusdir, "musicxml"))

    # load notes
    notes = getpiece(pieceid, :notes, :musicxml, type=:notes, keepids=true, unfold=true)
    timesig = getpiece(pieceid, :timesigs, :musicxml)[1]

    notedict = mknotedict(notes)

    groups = loadgroups(pieceid, schemaid, joinpath(corpusdir, "groups"), notedict, pick=true)
    suggested = [inst for group in groups for inst in group]
    positives = loadannots(pieceid, schemaid, joinpath(corpusdir, "annotations"), notedict, pick=true)

    negatives = setdiff(suggested, positives)
    return positives, negatives
end

function loadcorpusdata(corpusdir, schemaid)
    usedir(joinpath(corpusdir, "musicxml"))
    pieces = allpieces()

    positives = []
    negatives = []

    for piece in pieces
        @info "loading piece $(piece)"
        pos, neg = loadpiecedata(corpusdir, piece, schemaid)
        append!(positives, pos)
        append!(negatives, neg)
    end

    return positives, negatives
end
