#!/bin/bash

set -u

if [[ $# != 3 ]]; then
    echo "Usage: $0 SETTINGS_FILE TRAIN_FILE TEST_FILE"
    exit 1
fi

#############
# Constants #
#############

PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
SETTINGS="$1"
TRAIN_FILE="$2"
TEST_FILE="$3"

# Source common.sh
. "$PRG_DIR/common.sh"

# Check if those files exist
if [[ ! -f $SETTINGS ]]; then
    fatalError "Settings file $SETTINGS does not exist"
fi
if [[ ! -f $TRAIN_FILE ]]; then
    fatalError "Train data set file $TRAIN_FILE does not exist"
fi
if [[ ! -f $TEST_FILE ]]; then
    fatalError "Test data set file $TEST_FILE does not exist"
fi

# Source settings
. "$SETTINGS"

########
# Main #
########

# Evaluate true candidates on train and test
TRAIN_OUTPUT="train_rand_signal.moses"
TEST_OUTPUT="test_rand_signal.moses"

set -x

${MOSES_BUILD}eval-candidate \
    -i "$TRAIN_FILE" \
    -y "true" \
    -Q 0 \
    -H $fitness_type > "$TRAIN_OUTPUT"

${MOSES_BUILD}eval-candidate \
    -i "$TEST_FILE" \
    -y "true" \
    -Q 0 \
    -H $fitness_type > "$TEST_OUTPUT"

set +x
