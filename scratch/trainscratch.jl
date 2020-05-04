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

df = loadcorpusdata(corpusdir, ismirschemas)

df = cleancorpusdata(df, lex)
df = findgroups(df)

dfu = upsample(df)
dfd = downsample(df)

runfeatures!(df, feats)
runfeatures!(dfu, feats)
runfeatures!(dfd, feats)

plotfeatures(df, feats)

model = fitmodel(df, feats)
modelu = fitmodel(dfu, feats)
modeld = fitmodel(dfd, feats)

addpredictions!(df, model);
addpredictions!(dfu, modelu);
addpredictions!(dfd, modeld);

showeval(df)
showeval(dfu)
showeval(dfd)
