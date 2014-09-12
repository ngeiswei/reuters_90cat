#!/bin/bash

set -u

if [[ $# != 3 ]]; then
    echo "Usage: $0 SETTINGS_FILE MOSES_OUTPUT_FILE TEST_FILE"
    exit 1
fi

#############
# Constants #
#############

PROG_PATH="$(readlink -f "$0")"
PROG_DIR="$(dirname "$PROG_PATH")"
SETTINGS="$1"
MOSES_OUTPUT="$2"
TEST_DATASET="$3"

# Source settings and common.sh
. "$SETTINGS"
. "$PROG_DIR/common.sh"

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
    -H $fitness_type > "$EVAL_OUTPUT"

set +x
