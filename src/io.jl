# Saving and Loading Polygrams
# ============================

savepolys(fn, polys) = FileIO.save(fn, "polygrams", polys)

loadpolys(fn) = FileIO.load(fn, "polygrams")

# function loadlexicon(fn)
#     json = JSON.parsefile(fn, dicttype=DataStructures.OrderedDict)
#     DataStructures.OrderedDict(name => map(midis,def) for (name,def) in json)
# end

function loadlexicon(fn)
    # converts lexicon entry to schema prototype
    function mkschema(def)
        # guess pitch type based on json value type
        if isa(def[1][1], Int)
            ints = map(midis, def)
        else
            ints = map(stage -> map(parsespelled, stage), def)
        end
        schemarep(collect(permutedims(hcat(ints...))); sortstages=false)
    end

    json = JSON.parsefile(fn, dicttype=DataStructures.OrderedDict)
    DataStructures.OrderedDict(name => mkschema(def) for (name, def) in json)
end

# helpers for saving 

lowerrational(r::Rational) = (numerator=numerator(r), denominator=denominator(r))
lowerrational(x) = x
liftrational(obj::AbstractDict) = if haskey(:num, obj) && haskey(:denom, obj)
    obj[:num] // obj[:denom]
elseif haskey(:numerator, obj) && haskey(:denominator, obj)
    obj[:numerator] // obj[:denominator]
else
    obj
end
liftrational(x) = x

JSON.lower(p::Pitch) = JSON.lower(p.pitch)
JSON.lower(i::MidiInterval) = i.interval
JSON.lower(i::MidiIC) = i.ic
JSON.lower(i::SpelledInterval) = JSON.lower((d=i.d,c=i.c))
JSON.lower(i::SpelledIC) = JSON.lower((f=i.fifth))

# function saveannots(pieceid, schemaid, polys, dir)
#     escid = replace(pieceid, r"[\\/]" => s"_")
#     fn = joinpath(dir, "$(escid)_$(schemaid).json")

#     loweredpolys = map(polys) do poly
#         map(poly) do stage
#             map(stage) do note
#                 (pitch = pitch(note),
#                  onset = lowerrational(onset(note)),
#                  offset = lowerrational(offset(note)))
#             end
#         end
#     end

#     open(fn, "w") do file
#         JSON.print(file, (piece=pieceid, schema=schemaid, instances=loweredpolys), 2)
#     end
# end

lowernote(note) = if ismissing(note.id) || note.id == nothing
    (pitch = pitch(note),
     onset = lowerrational(onset(note)),
     offset = lowerrational(offset(note)))
else
    id(note)
end

function annotfilename(pieceid, schemaid, dir="")
    escid = replace(pieceid, r"[\\/]" => s"_")
    joinpath(dir, schemaid, "$(escid)_$(schemaid).json")
end

function saveannots(pieceid, schemaid, polys, dir)
    fn = annotfilename(pieceid, schemaid, dir)
    
    loweredpolys = map(polys) do poly
        map(poly) do stage
            map(lowernote, stage)
        end
    end

    unique!(loweredpolys)

    mkpath(dirname(fn))
    open(fn, "w") do file
        JSON.print(file, (piece=pieceid, schema=schemaid, instances=loweredpolys), 2)
    end
end

function loadannots(pieceid, schemaid, dir, notedict)
    fn = annotfilename(pieceid, schemaid, dir)
    
    annots = JSON.parsefile(fn)
    map(annots["instances"]) do instance
        map(instance) do stage
            map(stage) do note
                if isa(note, String)
                    notedict[note]
                else
                    TimedNote(note[:pitch],
                              liftrational(note[:onset]),
                              liftrational(note[:offset]))
                end
            end
        end
    end
end

# groups
########

function savegroups(pieceid, schemaid, groups, dir)
    fn = annotfilename(pieceid, schemaid, dir)
    
    loweredgroups = map(groups) do group
        map(group) do poly
            map(poly) do stage
                map(lowernote, stage)
            end
        end
    end

    unique!(loweredgroups)

    mkpath(dirname(fn))
    open(fn, "w") do file
        JSON.print(file, (piece=pieceid, schema=schemaid, groups=loweredgroups), 0)
    end
end
