cd flared/src/main/resources
gunzip nkjp1M.json.gz 
cd -


cd data/schemas-raw-corpuses/walenty-examples
gunzip walenty_examples_20160205.txt.disamb.chunked
cd -

cd data/schemas-raw-corpuses/eval-corpus
gunzip eval-corpus.txt.disamb.chunked.gz
cd -

# TODO: download models and unzip them
# compile sr and copy jar to flared/lib
