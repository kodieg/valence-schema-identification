#!/bin/bash

cd flared

if [ "$#" -eq 0 -o "$1" = "--help" ]; then
    echo "$0 output-path [input-path]" 
    exit 1;
fi

FEATPATH=data/featsSets/all.txt
DESC=`basename $FEATPATH`
ALGO=CRF-L1
COST=1.0
LABPROC=i
MINFREQ=1
INPUT=$2
OUTPUT=../$1

if [ -z $2 ]; then
    INPUT="data/Skladnica-full-pantera.dat"
else
    INPUT=../$INPUT
fi


sbt "runMain main.pas.RunCRFGridEvaluation $DESC $FEATPATH $ALGO $COST $MINFREQ $LABPROC pantera no-cv $OUTPUT $INPUT" 

echo "Model stored in $OUTPUT"
echo "To use this model move it to data/models/model.crf"
