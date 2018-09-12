#!/bin/bash

if [ "$#" -eq 0 -o "$1" = "--help" ]; then
    echo "$0 input-file/directory output";
    exit 1;
fi

INPUT=$1
OUTPUT=$2

if [ ! -e "$INPUT" ]; then
    echo "File '$INPUT' does not exists"
    exit 1
fi

function realpath { 
	case $1 in
	  /*) echo $1;;
	  *) echo `pwd`/$1;;
	esac	
}

INPUT=`realpath $INPUT`
OUTPUT=`realpath $OUTPUT`

cd flared

DATADIR=../data/
CACHEDIR=$DATADIR/sr-cache/

if [ ! -e "$DATADIR/sr/data.txt" ]; then
    echo "Please run ./train_valence_selection_models.sh chunked_nkjp_300m_path"
    exit 1;
fi

if [ -f "$DATADIR/ml/rf-models/models.txt" ]; then
    cd ../python-scripts/
    echo "Executing RF models as http services"
    bash run_server.sh ../$DATADIR/ml/rf-models/models.txt &
    RF_SERVER=$!
    cd ../flared
else
    echo "RF models not trained (WILL RUN WITHOUT THEM!)"
    RF_SERVER=
fi

echo -e "Identifying valence schemas in '$INPUT'\nStoring result in $OUTPUT."


ALGO="max:false,lex:true,sr:clark:false:0.3:false,vote:1.9:bayes:true:0.1:20.0:false;max:true:true;sr:splitsum:false:0.2:false;srone:splitsum:90:90:true,maxNoXP:true,negbayes:true:0.4:50.0:false,lex:false,bayes:true:0.1:20.0:false,srone:splitsum:90:90,vote:0.8:bayes:true:0.2:20.0:false;bayes:true:0.2:50.0:false;max:true;maxNoXP:false;sr:clark:true:0.1:false,vec:logreg:0.1:20.0:true,vote:1.5:bayes:true:0.2:50.0:false;http:0.1:0.8:cache;maxNoXP:true;srone:splitsum:10:90;srone:splitsum:90:90:true,vec:logreg:0.1:100.0,vote:1.0:bayes:true:0.1:20.0:false;max:false:true;sr:clark:true:0.1:false;vec:logreg:0.2:100.0;vec:logreg:0.5:100.0,max:true:true"


mkdir -p $CACHEDIR

PARAMS=" --wordnet-cache-file=$CACHEDIR/wordnet.bin"
PARAMS="$PARAMS --counts-cache-file=$CACHEDIR/counts.bin"
PARAMS="$PARAMS --corpus-path=$DATADIR/sr/data.txt"
PARAMS="$PARAMS --offline-limiter=$CACHEDIR/offline.limiter"
PARAMS="$PARAMS --treebank=$DATADIR/sr/Skladnica-zaleznosciowa-0.5.conll"
PARAMS="$PARAMS --verbs-counts=$CACHEDIR/frames.verbs"
PARAMS="$PARAMS --verbs-groups-counts=$CACHEDIR/frames.verbGroups"
PARAMS="$PARAMS --testset-cache=$CACHEDIR/testset.bin"

sbt "runMain main.pas.EvaluateFrameMatchingWith --input $INPUT --algo $ALGO $PARAMS"

if [ -n "$RF_SERVER" ]; then
    kill "$RF_SERVER"
fi
