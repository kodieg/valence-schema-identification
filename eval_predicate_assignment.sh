#!/bin/bash

cd flared

if [ "$1" = "--help" ]; then
    echo "$0 [input-path]" 
    exit 1;
fi

if [ -z $1 ]; then
    INPUT="data/Skladnica-full-pantera.dat"
else
    INPUT=../$INPUT
fi

sbt "runMain experimental.MainTestAssignments --input $INPUT --report-output ../data/args-report.html --output-eval"
