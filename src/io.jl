# Saving and Loading Polygrams
# ============================

savepolys(fn, polys) = FileIO.save(fn, "polygrams", polys)

loadpolys(fn) = FileIO.load(fn, "polygrams")

function loadlexicon(fn)
    json = JSON.parsefile(fn, dicttype=DataStructures.OrderedDict)
    DataStructures.OrderedDict(name => map(midis,def) for (name,def) in json)
end

# helpers for saving 

lowerrational(r::Rational) = (num=numerator(r), denom=denominator(r))
lowerrational(x) = x

JSON.lower(p::MidiPitch) = p.pitch

function saveannots(pieceid, schemaid, polys, dir)
    escid = replace(pieceid, r"[\\/]" => s"_")
    fn = joinpath(dir, "$(escid)_$(schemaid).json")

    loweredpolys = map(polys) do poly
        map(poly) do stage
            map(stage) do note
                (pitch = pitch(note),
                 onset = lowerrational(onset(note)),
                 offset = lowerrational(offset(note)))
            end
        end
    end

    open(fn, "w") do file
        JSON.print(file, (piece=pieceid, schema=schemaid, instances=loweredpolys), 2)
    end
end
