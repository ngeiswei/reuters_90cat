#!/bin/bash

set -u

if [[ $# != 3 ]]; then
    echo "Usage: $0 SETTINGS_FILE MOSES_OUTPUT_FILE TEST_FILE"
    exit 1
fi

#############
# Constants #
#############

PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
SETTINGS="$1"
MOSES_OUTPUT="$2"
TEST_DATASET="$3"

# Source common.sh
. "$PRG_DIR/common.sh"

# Check if those files exist
if [[ ! -f $SETTINGS ]]; then
    fatalError "Settings file $SETTINGS does not exist"
fi
if [[ ! -f $MOSES_OUTPUT ]]; then
    fatalError "MOSES output file $MOSES_OUTPUT does not exist"
fi
if [[ ! -f $TEST_DATASET ]]; then
    fatalError "Test data set file $TEST_DATASET does not exist"
fi

# Source settings
. "$SETTINGS"

########
# Main #
########

# Parse combo programs from the models file
MODELS="models.combo"
readonly moses_output_re="^($float_re) (.+) $composite_score_re"
while read result; do
    if [[ $result =~ $moses_output_re ]]; then
        echo ${BASH_REMATCH[2]}
    else
        fatalError "Moses output could not be parsed, there must be a bug"
    fi
done < "$MOSES_OUTPUT" > "$MODELS"

# Evaluate all combo programs on test
EVAL_OUTPUT="test.moses"

set -x

eval-candidate \
    -i "$TEST_DATASET" \
    -C "$MODELS" \
    -Q 0 \
    -W 1 \
    -H $fitness_type > "$EVAL_OUTPUT"

set +x
