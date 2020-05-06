using StatsBase
using Distances

### dependencies and export

export getduration, voicedist, stageskip
export rhythmDistanceSumInEvent, rhythmDistanceSumInVoice
export rhythmStageTransitionDistSum, rhythmVoiceTransitionDistSum


### VANILLA FEATURES

### Total duration of notes
function getDuration(poly, beatfactor)
	return beatfactor * Polygrams.totalduration(poly)
end

### Voice dist
function voiceDist()
	return Polygrams.voicedist
end

### Stage skip
function stageSkip(poly, beatfactor)
	return beatfactor * Polygrams.instageskip(poly)
end


### RHYTHMIC FEATURES

### Sum of the rhythmic distance between notes of the same event (by onset)
function rhythmDistanceSumInEvent(poly, beatfactor)
    # initialize sum and counter
    sum = 0
    npairs = 0

    # go over all events
    for event in poly
        nv = length(event)

        # sum over all pairs in that event
	for i = 1:nv-1
	    for j = i+1:nv
                dist = abs(onset(event[i]) - onset(event[j]))
                if !ismissing(dist)
                    npairs = npairs + 1
		    sum = sum + dist
                end
	    end
	end
        
    end
    
    return beatfactor * (1/npairs) * sum #Normalisation per event
end


### Sum of the rythmic distance between notes of the same voice (by onset)
function rhythmDistanceSumInVoice(poly, beatfactor)
    # initialize sum and counter
    sum = 0
    npairs = 0

    # dimensions
    nbevent = length(poly)
    nbvoice = length(poly[1])

    # go over all voices
    for i = 1:nbvoice

        #sum over all pairs in each voice
	for j = 1:nbevent-1
	    for k = j+1:nbevent
                dist = abs(onset(poly[j][i]) - onset(poly[k][i]))
                if !ismissing(dist)
                    npairs = npairs + 1
		    sum = sum + dist
                end
	    end
	end
    end
    
    return beatfactor * (1/npairs) * sum
end

"""
    rhythmicdist(stage1, stage2)

Measures the rhythmic dissimilarity between two stages,
which is defined as the the sum of absolute distance between corresponding notes
for the best alignment of the stages: min_o(∑_i |s1_i - s2_i - o|).
Returns a pair of the unnormalized distance and the number of note pairs.
"""
function rhythmicdist(stage1, stage2)
    n = length(stage1)
    npairs = 0
    mindist = Inf

    for j in 1:n
        offset = onset(stage2[j]) - onset(stage1[j])
        if !ismissing(offset)
            npairs = npairs + 1
            dist = sum(abs(onset(stage2[i]) - onset(stage1[i]) - offset)
                       for i in 1:n
                       if !ismissing(stage1[i]) & !ismissing(stage2[i]))
            if dist < mindist
                mindist = dist
            end
        end
    end

    if mindist == Inf
        mindist = 0
        npairs = 0
    end
    
    return mindist, npairs
end

"""
    rhythmicirregularity(poly, beatfactor)

Measures the rhythmic regularity of a polygram.
The regularity is defined as the sum of rhythmic dissimilarity
(as defined by `rhythmicdist`) between all pairs of stages.
The resulting value is normalized by the number of note pairs involved
and scaled by `beatfactor`.
"""
function rhythmicirregularity(poly, beatfactor)
    n = length(poly)
    npairs = 0
    sum = 0

    for i in 1:n-1
        for j in i+1:n
            d, p = rhythmicdist(poly[i], poly[j])
            sum = sum + d
            npairs = npairs + p
        end
    end

    reg = beatfactor * sum / npairs
    # if isinf(reg)
    #     @warn "infinite reg: $beatfactor, $sum, $npairs"
    # end

    return reg
end

### Vertical Symetricity of rhythmic distance from one event to another (by onset)
function rhythmStageTransitionDistSum(poly, beatfactor)
	dists = []
	for event in poly
		eventsum = 0
		N = size(event)[1]
	
		for i = 1:N-1
			for j = i+1:N
				eventsum = eventsum + abs(event[i].onset-event[j].onset)
			end
		end
		push!(dists, eventsum)
	end
	
	M = size(dists)[1]
	distsum = 0
	for i = 1:M-1
		for j = i+1:M
			distsum = distsum + abs(dists[i]-dists[j])
		end
	end
	distsum = beatfactor * (1/M)*distsum		#Normalisation per event

	return distsum
end


### Horizontal Symetricity of rhythmic distance from one voice to another (by onset)
function rhythmVoiceTransitionDistSum(poly, beatfactor)
	nbevent = size(poly)[1]
	nbvoice = size(poly[1])[1]

	distonsets = []
	for i = 1:nbvoice
		voiceonsets = []
		for event in poly
			push!(voiceonsets, event[i].onset)
		end

		dists = []
		for j = 1:nbevent-1
			for k = j+1:nbevent
				push!(dists, abs(voiceonsets[j]-voiceonsets[k]))
			end
		end
		push!(distonsets, dists)
	end

	distsum = 0
	K = size(distonsets[1])[1]

	for i = 1:K
		temp = 0
		for j = 1:nbvoice-1
			for k = j+1:nbvoice
				temp = temp + abs(distonsets[j][i] - distonsets[k][i])
			end
		end
		temp = (1/K)*temp
		distsum = distsum + temp
	end

	distsum = beatfactor * (1/nbvoice)*distsum

	return(distsum)
end


### PITCH FEATURES

### Sum of the pitch distance between notes of the same event (in semitones)
function pitchDistanceSumInEvent(poly)
    # initialize sum and counter
    sum = 0
    npairs = 0

    # go over all events
    for event in poly
        nv = length(event)

        # sum over all pairs in that event
	for i = 1:nv-1
	    for j = i+1:nv
		if !ismissing(event[i]) & !ismissing(event[j])
                    dist = abs(Int(tomidi(pitch(event[i]) - pitch(event[j]))))
                    npairs = npairs + 1
		    sum = sum + dist
                end
	    end
	end
        
    end
    
    return (1/npairs) * sum #Normalisation per event
end


### Sum of the pitch distance between notes of the same voice (in semitones)
function pitchDistanceSumInVoice(poly)
    # initialize sum and counter
    sum = 0
    npairs = 0

    # dimensions
    nbevent = length(poly)
    nbvoice = length(poly[1])

    # go over all voices
    for i = 1:nbvoice

        #sum over all pairs in each voice
	for j = 1:nbevent-1
	    for k = j+1:nbevent
		if !ismissing(poly[j][i]) & !ismissing(poly[k][i])
                    dist = abs(Int(tomidi(pitch(poly[j][i]) - pitch(poly[k][i]))))
                    npairs = npairs + 1
		    sum = sum + dist
                end
	    end
	end
    end
    
    return (1/npairs) * sum
end



### Vertical Symetricity of pitch distance from one event to another (in semitones)
function pitchStageTransitionDistSum(poly)
	dists = []
	for event in poly
		eventsum = 0
		N = length(event)
	
		for i = 1:N-1
			for j = i+1:N
				eventsum = eventsum + abs(Int(tomidi(pitch(event[i]) - pitch(event[j]))))
			end
		end
		push!(dists, eventsum)
	end
	
	M = length(dists)
	distsum = 0
	for i = 1:M-1
		for j = i+1:M
			distsum = distsum + abs(dists[i]-dists[j])
		end
	end
	distsum = (1/M)*distsum		#Normalisation per event

	return distsum
end


### Horizontal Symetricity of pitch distance from one voice to another (in semitones)
function pitchVoiceTransitionDistSum(poly)
	nbevent = size(poly)[1]
	nbvoice = size(poly[1])[1]

	distpitches = []
	for i = 1:nbvoice
		voicepitches = []
		for event in poly
			push!(voicepitches, pitch(event[i]))
		end

		dists = []
		for j = 1:nbevent-1
			for k = j+1:nbevent
				push!(dists, abs(Int(tomidi(voicepitches[j]-voicepitches[k]))))
			end
		end
		push!(distpitches, dists)
	end

	distsum = 0
	K = length(distpitches[1])

	for i = 1:K
		temp = 0
		for j = 1:nbvoice-1
			for k = j+1:nbvoice
				temp = temp + abs(distpitches[j][i] - distpitches[k][i])
			end
		end
		temp = (1/K)*temp
		distsum = distsum + temp
	end

	distsum = (1/nbvoice)*distsum

	return(distsum)
end

### pitchclass histogram features

function pchist(notes, weighted=false; normalize=true)
    pitches = pc.(pitch.(notes))
    counts = Dict{Pitch{SpelledIC},Float64}()
    
    if weighted
        ws = weights(convert.(Float64, duration.(notes)))
        norm = sum(ws)
    else
        norm = length(notes)
        ws = uweights(Float64, norm)
    end
    
    addcounts!(counts, pitches, ws)

    if normalize
        map!(x -> x / norm, values(counts)) # this should update the values in place
    end

    counts
end

function contexthists(df, weighted=true)
    function contexthist(subdf)
        poly = subdf.notes[1]
        histpc = pchist(subdf.context[1], weighted)

        refnote = poly[1][findfirst(!ismissing, poly[1])]
        ref = pc(pitch(refnote))
        histic = [(p-ref, v) for (p,v) in histpc]
        return (ic=getindex.(histic,1), freq=getindex.(histic,2))
    end
    
    by(df, [:notes, :schema], [:context, :notes] => contexthist)
end

#mergehists(h1, h2) = merge(+, h1, h2)

function schemahists(polyhistdf)
    by(polyhistdf, [:schema, :ic], freq=:freq=>mean)
end

schemaprofiles(shdf) = shdf[shdf.isinstance, Not(:isinstance)]

function profiledist(profile, observed, dist=Euclidean())
    joint = join(profile, observed, on=:ic, makeunique=true, kind=:outer)
    
    evaluate(dist, coalesce.(joint.freq, 0), coalesce.(joint.freq_1, 0))
end

function contextfeature(ctxdf, profiledf, dist=Euclidean())
    profs = Dict(schema => profiledf[profiledf.schema .== schema, :]
                 for schema in levels(profiledf.schema))
    
    function evaldist(cols)
        profile = profs[cols.schema[1]]
        profiledist(profile, DataFrame(ic=cols.ic, freq=cols.freq), dist)
    end

    by(ctxdf, :notes, profiledist=[:ic, :freq, :schema]=>evaldist)
end

function plotichists(hists; groups=1:length(hists), kwargs...)
    fifths = [ic.fifths for h in hists for ic in keys(h)]
    xs = sic.(minimum(fifths):maximum(fifths))
    ys = hcat([[get(h, ic, 0) for ic in xs] for h in hists]...)
    group=repeat(groups, inner=length(xs))

    groupedbar(repeat(string.(xs), outer=length(hists)), ys; group=group, kwargs...)
end
