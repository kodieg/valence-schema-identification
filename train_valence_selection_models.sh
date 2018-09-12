#!/bin/bash

#cd flared

INPUT=$1
DATADIR=../data/
CACHEDIR=$DATADIR/sr-cache/

if [ -e "$DATADIR/sr/data.txt" ]; then
    echo "File: flared/$DATADIR/sr/data.txt already exists!" 
    exit 1;
else
    touch $DATADIR/sr/data.txt
fi

cd ..
mkdir -p data/binary
rm -rf ./data/binary/arguments-nkjp-300m
./identify_arguments.sh "$INPUT" ./data/binary/arguments-nkjp-300m

cd flared


ALGO="max:false,maxNoXP:false"

mkdir -p $CACHEDIR


PARAMS=" --wordnet-cache-file=$CACHEDIR/wordnet.bin"
PARAMS="$PARAMS --counts-cache-file=$CACHEDIR/counts.bin"
PARAMS="$PARAMS --corpus-path=$DATADIR/sr/data.txt"
PARAMS="$PARAMS --offline-limiter=$CACHEDIR/offline.limiter"
PARAMS="$PARAMS --treebank=$DATADIR/sr/Skladnica-zaleznosciowa-0.5.conll"
PARAMS="$PARAMS --verbs-counts=$CACHEDIR/frames.verbs"
PARAMS="$PARAMS --verbs-groups-counts=$CACHEDIR/frames.verbGroups"
PARAMS="$PARAMS --testset-cache=$CACHEDIR/testset.bin"

echo "*** EXTRACTING DATA FOR SELECTIONAL RESTRICTIONS **"

sbt "runMain main.pas.ExtractCorpusForSRWith --input ../data/binary/arguments-nkjp-300m --output $DATADIR/sr/data.txt.tmp --algo $ALGO $PARAMS"

mv $DATADIR/sr/data.txt.tmp $DATADIR/sr/data.txt
 
# Force regenerate caches
rm -rf $DATADIR/sr-cache
mkdir -p $DATADIR/sr-cache

bash ../data/gen-caches.sh $DATADIR/sr/data.txt $DATADIR/sr-cache

mkdir -p $DATADIR/ml/

echo "*** EXTRACTING DATA FOR RANDOM FORREST ***"

sbt "runMain main.pas.ExtractCorpusForML --input ../data/binary/arguments-nkjp-300m --algo $ALGO --output $DATADIR/ml/rf-data $PARAMS"

echo "*** SPLITTING TRAINING DATA PER PREDICTE ***"

sbt -DrunMain.memory="80G" "runMain experimental.MLCorpusToScikitCSV --input $DATADIR/ml/rf-data --output $DATADIR/ml/rf-csv $PARAMS"

echo "*** TRAINING RANDOM FORRESTS ***" 
cd ../python-scripts
rm -rf $DATADIR/ml/rf-models
mkdir -p $DATADIR/ml/rf-models
./train-rf.py $DATADIR/ml/rf-csv $DATADIR/ml/rf-models


cd ../
echo "*** BUILDING EVAL CORPUSES ***" 
./annotate_corpus.sh ./data/schemas-corpuses/eval-corpus/annotated-eval-corpus.obj ./data/models/model.crf ./data/schemas-raw-corpuses/eval-corpus/eval-corpus.txt.disamb.chunked ./data/schemas-raw-corpuses/eval-corpus/eval-corpus.csv 

echo "*** BUILDING WALENTY CORPUSES ***" 
./annotate_corpus.sh ./data/schemas-corpuses/walenty-corpus/annotated-walenty-corpus.obj ./data/models/model.crf ./data/schemas-raw-corpuses/walenty-examples/walenty_examples_20160205.txt.disamb.chunked ./data/schemas-raw-corpuses/walenty-examples/walenty_detailed_examples_20160205.csv



