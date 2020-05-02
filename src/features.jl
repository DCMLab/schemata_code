### dependencies and export


export getduration, voicedist, stageskip
export rhythmDistanceSumInEvent, rhythmDistanceSumInVoice
export rhythmStageTransitionDistSum, rhythmVoiceTransitionDistSum


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

function rhythmicdist(stage1, stage2)
    n = length(stage1)
    npairs = 0
    mindist = Inf

    for j in 1:n
        offset = onset(stage2[j]) - onset(stage1[j])
        if !ismissing(offset)
            npairs = npairs + 1
            dist = sum(onset(stage2[i]) - onset(stage1[i]) - offset
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
function rhythmStageTransitionDistSum(poly)
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
	distsum = (1/M)*distsum		#Normalisation per event

	return distsum
end


### Horizontal Symetricity of rhythmic distance from one voice to another (by onset)
function rhythmVoiceTransitionDistSum(poly)
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

	distsum = (1/nbvoice)*distsum

	return(distsum)
end
