using DigitalMusicology: MidiPitch, midi

const SchemaType = Vector{Vector{MidiPitch}}

macro schema(name, variant, stages)
    quote
        if !isdefined(quote $(esc(name)) end)
            $(esc(name)) = Dict{String, SchemaType}()
        end
        let schema = $stages
            if validateschema(schema)
                $(esc(name))[$variant] = mkschema(schema)
            end
        end
    end
end

function mkschema(stages)
    ref = stages[1][1]
    map(notes -> map(n -> pc(midi(n-ref)), notes), stages)
end

validateschema(stages) = all(s -> length(s) == length(stages[1]), stages)
