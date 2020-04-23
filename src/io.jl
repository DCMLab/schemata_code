using DigitalMusicology
import JSON
import DataStructures

const DS = DataStructures

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
raiserational(obj::AbstractDict) = if haskey(:num, obj) && haskey(:denom, obj)
    obj[:num] // obj[:denom]
elseif haskey(:numerator, obj) && haskey(:denominator, obj)
    obj[:numerator] // obj[:denominator]
else
    obj
end
raiserational(x) = x

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

# handling several instances of the same note
#############################################

"""
    mknotedict(notes)

Takes a collection of `TimedNote`s and groups them into a dictionary by their `id`s.
Each entry has the `id` as its key and a `Set` of `TimedNote`s with that ID as its value.
"""
function mknotedict(notes)    
    notedict = Dict()
    
    for note in notes
        id = note.id
        if haskey(notedict, id)
            push!(notedict[id], note)
        else
            notedict[id] = Set([note])
        end
    end

    return notedict
end

"""
    picktinstance(instance)

Takes a schema instance that has several candidate notes per note slot.
Returns an instances with exactly one note per slot and minimal duration.
"""
function pickinstance(instance)
    # track all combinations of notes as "prefixes"
    # each prefix is named tuple with the following fields
    #   notes: picked so far (in reverse order)
    #   on:    the total onset of that combination,
    #   off:   the total offset of that combination,
    #   after: the onset of the previous stage (`nothing` for the first stage)
    prefixes = nothing

    # track if we are at the first slot
    firstslot = true

    # go through all slots and pick every possible note
    for stage in instance
        for slot in stage
            if firstslot
                prefixes = [(notes=DS.list(note),
                             on=note.onset,
                             off=note.offset,
                             after=nothing)
                            for note in slot]
            else
                prefixes = [(notes=DS.cons(note, pfx.notes),
                             on=min(pfx.on, note.onset),
                             off=max(pfx.off, note.offset),
                             after=pfx.after)
                            for pfx in prefixes
                            for note in slot
                            if isnothing(pfx.after) || note.onset > pfx.after]
            end
            
            firstslot = false
        end
        
        # end of stage: update the lower bound for the next stage in each prefix
        prefixes = map(prefixes) do pfx
            (pfx..., after=pfx.on)
        end
    end

    # pick prefix with minimal cost
    # manual argmin bc. lack of a proper argmin in Julia
    mincost = nothing
    minpfx = nothing
    for pfx in prefixes
        cost = pfx.off - pfx.on
        if isnothing(mincost) || cost < mincost
            mincost = cost
            minpfx = pfx
        end
    end

    # convert prefix to a proper instance:
    # go through the input instance and always pick the next note from the list 
    notes = reverse(minpfx.notes)
    pickedinst = map(instance) do stage
        map(stage) do _
            note = notes.head
            notes = notes.tail
            note
        end
    end
    
    return pickedinst
end

# annotations
#############

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

function loadannots(pieceid, schemaid, dir, notedict; pick=false)
    fn = annotfilename(pieceid, schemaid, dir)
    
    annots = JSON.parsefile(fn)
    map(annots["instances"]) do instance
        inst = map(instance) do stage
            map(stage) do note
                if isa(note, String)
                    notedict[note]
                else
                    TimedNote(note[:pitch],
                              raiserational(note[:onset]),
                              raiserational(note[:offset]))
                end
            end
        end
        if pick
            pickinstance(inst)
        else
            inst
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

function loadgroups(pieceid, schemaid, dir, notedict; pick=false)
    fn = annotfilename(pieceid, schemaid, dir)

    groupsjson = JSON.parsefile(fn)
    map(groupsjson["groups"]) do group
        map(group) do instance
            inst = map(instance) do stage
                map(stage) do note
                    if isa(note, String)
                        notedict[note]
                    else
                        TimedNote(note[:pitch],
                                  raiserational(note[:onset]),
                                  raiserational(note[:offset]))
                    end
                end
            end
            if pick
                pickinstance(inst)
            else
                inst
            end
        end
    end
end
