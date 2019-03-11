cd flared/src/main/resources
gunzip nkjp1M.json.gz 
cd -


cd data/schemas-raw-corpuses/walenty-examples
gunzip walenty_examples_20160205.txt.disamb.chunked
cd -

cd data/schemas-raw-corpuses/eval-corpus
gunzip eval-corpus.txt.disamb.chunked.gz
cd -


wget 'http://mozart.ipipan.waw.pl/~kodie/models-nkjp-300m.tgz'
tar xzvf models-nkjp-300m.tgz
# TODO: download models and unzip them
# compile sr and copy jar to flared/lib

cd flared/data
wget 'http://mozart.ipipan.waw.pl/~kodie/300m.bin'

