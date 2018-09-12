#!/bin/bash


function realpath { 
	case $1 in
	  /*) echo $1;;
	  *) echo `pwd`/$1;;
	esac	
}

if [ "$#" -eq 0 -o "$1" = "--help" ]; then
    echo "$0 output-path model-path input-path-chunked input-path-csv" 
    exit 1;
fi

FEATPATH=data/featsSets/all.txt
DESC=`basename $FEATPATH`
ALGO=CRF-L1
COST=1.0
MINFREQ=1
LABPROC=I
# MODEL_PATH=data/crf-grid-eval/crf-all.txt-a-CRF-L1-c-1.0-minFreq-1-label-P/model.crf
MODEL_PATH=`realpath $2`
MODEL_DIR=`dirname $MODEL_PATH`
KEEPLABELS=2
KEEPTHR=0.9
USECP=true

# TODO: test KEEPLABELS = 1 AND KEEPTHR == 0.0
#DESC="feats"
#FEATPATH="feats.txt"
#ALGO="CRF"
#COST="1.0"
#MINFREQ="10"
#LABPROC="Identity"
#KEEPLABELS="3"
#KEEPTHR="0.6"
#USECP="true"
# MODEL_PATH="/home/kodie/Flared/data/crf-grid-eval/crf-feats-a-CRF-c-1.0-minFreq-10-label-I-keep-3-th-0.6-cp-true/model.crf"
OUT_PATH=`realpath $1`
CHUNKED_PATH=`realpath $3`
CSV_PATH=`realpath $4`

cd flared

sbt "runMain main.pas.RunAnnotateWalentyExamples $OUT_PATH $MODEL_PATH $ALGO $COST $MINFREQ $LABPROC $KEEPLABELS $KEEPTHR $USECP $CHUNKED_PATH $CSV_PATH"
