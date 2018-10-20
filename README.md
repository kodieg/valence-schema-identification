= Argument and valency schema identification toolkit =

Model for argument identification is located in `data/models/model.crf`, so argument identification task can be performed without additional training. In order to identify arguments use `identify_arguments.sh` script. To train new model use `train_crf_model.sh` script. Before using valency identification scripts one need to train valency schema models using `train_valence_selection_models.sh` 

Before using this tool download Słowosieć from http://nlp.pwr.wroc.pl/plwordnet/download/ and save it to `flared/src/main/resources/plwordnet_2_0.xml`.
Then run install.sh script

== `identify_arguments.sh` script ==

Command parameters:
`identify_arguments.sh [input-file-chunked.ccl] [output-file]`

This script will identify arguments of predicates in the input file and store the result in output file that can be used for schema identification.

Input file must be the result of using iobber with syntactic head model.
Output will have binary format.

Example:
    `./identify_arguments.sh data/samples/sample-data.chunked example.obj`

== `train_crf_model.sh` script ==

Command parameters:
`train_crf_model.sh output-path [input-path]`

Trains model. By default, uses included Skladnica dataset.

Example:
    `./train_crf_model.sh new-model.crf`


== `train_valence_selection_models.sh` command ==

Command parameters:
    `./train_valence_selection_models.sh [dataset-chunked]`

Use to train default valence selection models. Take path to directory containing texts chunked by iobber (and probably annotated morphosyntactically by Pantera). NKJP300M was used for training models in the conducted experiments.


== `identify_schemas.sh` command ==

Command parameters:
`identify_schemas.sh input-directory output-file`

Identifies schemas in the text with identified arguments (i.e. result of `identify_arguments.sh` command). Outputs CONLL format.

Example:
    `./identify_schemas.sh example.obj example.conll`


== `annotate_corpus.sh` command ==

Command used to turn walenty examples corpus and evaluation corpus to corpus that `eval_valency_schema_selection.sh` can handle.


== `eval_*.sh` scripts ==

Those are scripts that perform evaluation of implemented methods.


License notes:
NKJP1M corpus converted to json is included within this repository. NKJP1M is available on GNU GPL v3 license.
