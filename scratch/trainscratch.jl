corpusdir = "../../schema_annotation_data/data/mozart_sonatas/"

lex = Polygrams.loadlexicon(joinpath(corpusdir, "..", "lexicon.json"))

ismirschemas = [
    "doremi.2",
    "fenaroli.2",
    "fenaroli.2.min",
    "fenaroli.2.basscanon",
    "fenaroli.2.basscanon.min",
    #"fenaroli.2.durante",
    #"fenaroli.2.durante.min",
    "fenaroli.2.flipped",
    "fenaroli.2.flipped.min",
    "fenaroli.2.melcanon",
    "fenaroli.2.melcanon.min",
    "folia.2",
    "fonte.2",
    "fonte.2.flipped",
    "fonte.2.majmaj",
    "grandcad.2",
    "indugio.2",
    "indugio.2.voiceex",
    "lamento.2",
    "lully.2",
    "morte.2",
    "prinner.2",
    "quiescenza.2",
    "quiescenza.2.diatonic",
    "solfami.2"
]

df, notelists = loadcorpusdata(corpusdir, ismirschemas);

df = cleancorpusdata(df, lex)
df = findgroups(df)
df = findfullcontexts(df, notelists)

df = runfeatures(df, feats)

info = trainfeatures(df)
df = rundepfeatures(df, info)

plotfeatures(df, featcols)

dfu = upsample(df)
dfd = downsample(df)

model = fitmodel(df, featcols)
modelu = fitmodel(dfu, featcols)
modeld = fitmodel(dfd, featcols)

df = addpredictions(df, model);
df = addpredictions(dfu, modelu);
df = addpredictions(dfd, modeld);

showeval(df)
showeval(dfu)
showeval(dfd)
