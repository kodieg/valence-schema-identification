#!/bin/bash

if [ "$#" -eq 0 -o "$1" = "--help" ]; then
    echo "$0 input-file/directory output";
    exit 1;
fi

function realpath { 
	case $1 in
	  /*) echo $1;;
	  *) echo `pwd`/$1;;
	esac	
}

INPUT=$1
OUTPUT=$2

if [ ! -e "$INPUT" ]; then
    echo "File '$INPUT' does not exists"
    exit 1
fi

INPUT=`realpath $INPUT`
OUTPUT=`realpath $OUTPUT`

cd flared

echo -e "Identifying arguments in '$INPUT'\nStoring result in $OUTPUT."

find $INPUT | grep chunked$ > data/input-files

# main.pas.ExtractStructureFromCCL read all files from `data/input-files`
MODEL_PATH=../data/models/model.crf

sbt "runMain experimental.ExtractStructureFromCCL $MODEL_PATH $OUTPUT"
