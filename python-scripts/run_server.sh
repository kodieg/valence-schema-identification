#/bin/bash

BASEDIR=.

for PORT in `seq 5100 5120`; do
	python $BASEDIR/model_server.py $1 $PORT &> log.${PORT}.txt & 
done

trap 'kill -9 $(jobs -pr)' SIGINT SIGTERM EXIT


for i in `seq 5100 5120`; do
  wait
done
