#!/bin/bash

data=$1
cache=$2
echo "Generating verb counts"
cat $data | cut -d '	' -f 1 | sort | uniq -c | sort | sed 's/^\s*//' > $cache/frames.verbs
echo "Generating verb group counts"
cat $data | cut -d '	' -f 1,5 | sort | uniq -c | sort | sed 's/^\s*//' > $cache/frames.verbGroups
echo "Generating offline.limiter file"
cat $cache/frames.verbs | cut -d ' ' -f 2- > $cache/offline.limiter
