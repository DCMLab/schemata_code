corpusdir = "../../schema_annotation_data/data/mozart_sonatas/"

lex = Polygrams.loadlexicon(joinpath(corpusdir, "..", "lexicon.json"))

ismirschemas = [
    "doremi.2", # 5
    "fenaroli.2", # 10
    #"fenaroli.2.min", # 3
    #"fenaroli.2.basscanon", # 0
    #"fenaroli.2.basscanon.min", # 1
    #"fenaroli.2.durante",
    #"fenaroli.2.durante.min",
    "fenaroli.2.flipped", # 43
    "fenaroli.2.flipped.min", # 8
    "fenaroli.2.melcanon", # 6
    #"fenaroli.2.melcanon.min", # 2
    #"folia.2", # 0
    "fonte.2", # 49
    #"fonte.2.flipped", # 2
    "fonte.2.majmaj", # 8
    #"grandcad.2", # 0
    "indugio.2", # 9
    "indugio.2.voiceex", # 5
    #"lamento.2", # 2
    #"lully.2", # 2
    #"morte.2", # 1
    "prinner.2", # 32
    "quiescenza.2", # 46
    "quiescenza.2.diatonic", # 6
    #"solfami.2" # 4
]

df, notelists = loadcorpusdata(corpusdir, ismirschemas);

df = cleancorpusdata(df, lex)
df = findgroups(df)
df = findfullcontexts(df, notelists)

#dffull = df
#df = df[map(s->s ∈ ismirschemas,df.schema), :]
#df = dffull

df = runfeatures(df, feats)

dftrain, dftest = splitdf(df)

info = trainfeatures(dftrain)
dftrain = rundepfeatures(dftrain, info)
dftest  = rundepfeatures(dftest, info)

###
stghists = stagehists(dftrain[dftrain.isinstance, :])
stgprofiles = stageprofiles(stghists)
testhists = stagehists(dftest)
###

plotfeatures(dftest, featcols)

#dfu = upsample(df)
#dfd = downsample(df)
dfutrain = upsample(dftrain)
dfutest  = upsample(dftest)
dfdtrain = downsample(dftrain)
dfdtest = downsample(dftest)

# logistic regression

model = fitmodel(dftrain, featcols)
modelu = fitmodel(dfutrain, featcols)
modeld = fitmodel(dfdtrain, featcols)

dftest = addpredictions(dftest, modelu);
dftestalt = addpredictions(dftest, model, corrinter=0);
dfutest = addpredictions(dfutest, modelu);
dfdtest = addpredictions(dfdtest, modeld);

# SVM

svm = fitsvm(dftrain, featcols)
#svmu = fitsvm(dfu, featcols)
svmd = fitsvm(dfdtrain, featcols)

dftest = predictsvm(dftest, svm, featcols)
dfutest = predictsvm(dfutest, svmd, featcols)
dfdtest = predictsvm(dfdtest, svmd, featcols)

# NN: LR

annlr, losses, testlosses =
    fitann(dfutrain, featcols;
           batchsize=1024,
           opt=ADAM(0.01),
           epochs=3,
           dftest=dfutest)
plot([losses testlosses])
dfdtest = predann(dfdtest, annlr, featcols, :nnlr)
dfutest = predann(dfutest, annlr, featcols, :nnlr)
dftest = predann(dftest, annlr, featcols, :nnlr)
plotcol(dfutest, :nnlr)

# NN: hidden layer

nfeat = length(featcols)
ann, losses, testlosses =
    fitann(dfutrain, featcols;
           batchsize=1024,
           opt=ADAM(0.01),
           epochs=3, # converges after ca 3 ep
           dftest=dfutest,
           model=Chain(Dense(nfeat, 2*nfeat, σ), Dense(2*nfeat, 1, σ)))
plot([losses testlosses])
dfdtest = predann(dfdtest, ann, featcols, :nn)
dfutest = predann(dfutest, ann, featcols, :nn)
dftest = predann(dftest, ann, featcols, :nn)
plotcol(dfutest, :nn)

# evaluation

showeval(dftest)
showeval(dfutest)
showeval(dfutest, predcol=:nnbool)
showeval(dfdtest)

evaltable(dftest)
evaltable(dfutest)
evaltable(dfdtest)

dfgrp = aggregategroups(df, :nnlr, 0.5) do g
    count(g .> 0.5) / length(g)
end
plotcol(dfgrp, :grppred, group=:groupisinstance)
#dfgrp[!, :grppredbool] = dfgrp.grpmax .> 0.75
showeval(dfgrp, gtcol=:groupisinstance, predcol=:grppredbool)

fss = []
for thres in 0:0.01:1
    gdf = aggregategroups(g -> count(g .> 0.5) / length(g),
                          df, :nnlr, thres)
    fs = @df gdf fscore(:groupisinstance, :grppredbool)
    push!(fss, fs)
end
plot(0:0.01:1, fss)

# finding interesting instances

fpsd = findfps(dfdtest)
fpsd = confidentfps(dfutest, gtcol=:groupisinstance, thres=0.9)

# testing

modelu = fitmodel(dfutrain, featcols)
dfutest = addpredictions(dfutest, modelu);
showeval(dfutest)

dfutrain = addpredictions(dfutrain, modelu);
showeval(dfutrain)

modeld = fitmodel(dfdtrain, featcols)
dfdtest = addpredictions(dfdtest, modeld);
showeval(dfdtest)
