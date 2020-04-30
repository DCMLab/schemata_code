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
function rhythmDistanceSumInEvent(poly)
	sum = 0
	M = size(poly)[1]	#Number of event in poly
	
	for event in poly
		eventsum = 0
		N = size(event)[1]	#Number of voices in poly
		
		for i = 1:N-1
			for j = i+1:N
				eventsum = eventsum + abs(event[i].onset - event[j].onset)
			end
		end
		eventsum = (2/(N*(N-1)))*eventsum 		#Normalisation per combinaison of voices
		sum = sum+eventsum
	end

	sum = (1/M)*sum		#Normalisation per event

	return sum
end


### Sum of the rythmic distance between notes of the same voice (by onset)
function rhythmDistanceSumInVoice(poly)
	sum = 0
	nbevent = size(poly)[1]
	nbvoice = size(poly[1])[1]

	for i = 1:nbvoice
		voiceonsets = []
		for event in poly
			push!(voiceonsets, event[i].onset)
		end
		
		voicesum = 0
		for j = 1:nbevent-1
			for k = j+1:nbevent
				voicesum = voicesum + abs(voiceonsets[j]-voiceonsets[k])
			end
		end
		voicesum = (2/(nbevent*(nbevent-1)))*voicesum	#Normalisation per combinaison of events
		sum = sum+voicesum
	end

	sum = (1/nbvoice)*sum		#Normalisation per number of voices

	return sum
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

	return(distsum)
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
