#!/bin/bash

set -u

if [[ $# != 3 ]]; then
    echo "Usage: $0 SETTINGS_FILE TRAIN_FILE RND_SEED"
    exit 1
fi

#############
# Constants #
#############

PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
SETTINGS="$1"
TRAIN_FILE="$2"
RND_SEED="$3"

# Source settings and common.sh
. "$SETTINGS"
. "$PRG_DIR/common.sh"

FS_LOG="feature_selection_$(chg_ext "$TRAIN_FILE" log)"
FS_OUTPUT="filtered_$TRAIN_FILE"

########
# Main #
########

set -x

feature-selection \
    -i "$TRAIN_FILE" \
    -o "$FS_OUTPUT" \
    -r $RND_SEED \
    -a $fs_algo \
    -H $fs_scorer \
    -C $fs_nfeats \
    -F "$FS_LOG" \
    -j $jobs \
    -l $log_level

set +x
